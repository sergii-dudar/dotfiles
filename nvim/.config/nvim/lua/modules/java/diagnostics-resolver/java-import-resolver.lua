--- Direct Java type-reference and import planning for generated diagnostic fixes.

local M = {}

local PRIMITIVES = {
    boolean = true,
    byte = true,
    char = true,
    double = true,
    float = true,
    int = true,
    long = true,
    short = true,
    void = true,
}

local JVM_PRIMITIVE_DESCRIPTORS = {
    B = "byte",
    C = "char",
    D = "double",
    F = "float",
    I = "int",
    J = "long",
    S = "short",
    Z = "boolean",
}

---@class JavaResolvedType
---@field className string
---@field packageName? string

---@class JavaTypeDescriptor
---@field canonical_name string
---@field class_reference string
---@field package_name string
---@field root_name string
---@field import_name? string
---@field primitive boolean

--- Convert a reflection class name into a Java source type descriptor.
---@param resolved JavaResolvedType
---@return JavaTypeDescriptor|nil descriptor
---@return string|nil error
function M.describe(resolved)
    if type(resolved) ~= "table" or type(resolved.className) ~= "string" or resolved.className == "" then
        return nil, "MapStruct returned no Java class name"
    end

    local class_name = resolved.className
    local dimensions = 0
    while class_name:sub(1, 1) == "[" do
        dimensions = dimensions + 1
        class_name = class_name:sub(2)
    end

    if dimensions > 0 then
        if class_name:sub(1, 1) == "L" and class_name:sub(-1) == ";" then
            class_name = class_name:sub(2, -2)
        else
            class_name = JVM_PRIMITIVE_DESCRIPTORS[class_name]
        end
        if not class_name then
            return nil, "Unsupported JVM type descriptor: " .. resolved.className
        end
    end

    local array_suffix = string.rep("[]", dimensions)
    if PRIMITIVES[class_name] then
        return {
            canonical_name = class_name .. array_suffix,
            class_reference = class_name .. array_suffix,
            package_name = "",
            root_name = class_name,
            primitive = true,
        }
    end

    local package_name = resolved.packageName or ""
    if package_name == "" then
        local inferred_package = class_name:match("^(.-)%.%u")
        package_name = inferred_package or ""
    end

    local binary_class_name = class_name
    local package_prefix = package_name ~= "" and (package_name .. ".") or ""
    if package_prefix ~= "" and vim.startswith(binary_class_name, package_prefix) then
        binary_class_name = binary_class_name:sub(#package_prefix + 1)
    end

    local source_class_name = binary_class_name:gsub("%$", ".")
    local root_name = source_class_name:match("^([^%.]+)") or source_class_name
    local canonical_name = package_prefix .. source_class_name .. array_suffix

    return {
        canonical_name = canonical_name,
        class_reference = source_class_name .. array_suffix,
        package_name = package_name,
        root_name = root_name,
        import_name = package_name ~= "" and (package_name .. "." .. root_name) or nil,
        primitive = false,
    }
end

--- Check that a backend type agrees with the type named by the diagnostic.
---@param resolved JavaResolvedType
---@param expected string
---@return boolean
function M.matches(resolved, expected)
    local descriptor = M.describe(resolved)
    if not descriptor or type(expected) ~= "string" then
        return false
    end

    expected = expected:gsub("%$", "."):gsub("%s+", "")
    local canonical_name = descriptor.canonical_name:gsub("%s+", "")
    local class_reference = descriptor.class_reference:gsub("%s+", "")
    if expected == canonical_name or expected == class_reference then
        return true
    end

    return #expected < #canonical_name and canonical_name:sub(-#expected - 1) == "." .. expected
end

---@param bufnr integer
---@return table environment
local function read_import_environment(bufnr)
    local environment = {
        package_name = "",
        direct_by_simple = {},
        direct = {},
        wildcard_packages = {},
    }

    for _, line in ipairs(vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)) do
        local package_name = line:match("^%s*package%s+([%w_%.]+)%s*;")
        if package_name then
            environment.package_name = package_name
        end

        if not line:match("^%s*import%s+static%s+") then
            local import_name = line:match("^%s*import%s+([%w_%.%*]+)%s*;")
            if import_name then
                if import_name:sub(-2) == ".*" then
                    environment.wildcard_packages[import_name:sub(1, -3)] = true
                else
                    environment.direct[import_name] = true
                    local simple_name = import_name:match("([^%.]+)$")
                    environment.direct_by_simple[simple_name] = import_name
                end
            end
        end
    end

    return environment
end

--- Plan source references and imports for resolved Java types without editing the buffer.
---@param bufnr integer
---@param resolved_types { key: string, type: JavaResolvedType }[]
---@return table<string, string>|nil references
---@return string[]|string imports_or_error
function M.plan(bufnr, resolved_types)
    local environment = read_import_environment(bufnr)
    local descriptors = {}
    local root_imports = {}

    for _, item in ipairs(resolved_types) do
        local descriptor, err = M.describe(item.type)
        if not descriptor then
            return nil, err
        end
        descriptors[#descriptors + 1] = { key = item.key, value = descriptor }

        if descriptor.import_name then
            root_imports[descriptor.root_name] = root_imports[descriptor.root_name] or {}
            root_imports[descriptor.root_name][descriptor.import_name] = true
        end
    end

    local references = {}
    local imports = {}
    local seen_imports = {}

    for _, item in ipairs(descriptors) do
        local descriptor = item.value
        local reference = descriptor.class_reference
        local import_name = descriptor.import_name

        if import_name then
            local existing = environment.direct_by_simple[descriptor.root_name]
            local desired_count = vim.tbl_count(root_imports[descriptor.root_name])
            local collision = (existing and existing ~= import_name) or desired_count > 1
            local implicitly_available = descriptor.package_name == "java.lang"
                or descriptor.package_name == environment.package_name
                or environment.wildcard_packages[descriptor.package_name]

            if collision then
                reference = descriptor.canonical_name
            elseif
                not implicitly_available
                and not environment.direct[import_name]
                and not seen_imports[import_name]
            then
                imports[#imports + 1] = import_name
                seen_imports[import_name] = true
            end
        end

        references[item.key] = reference
    end

    table.sort(imports)
    return references, imports
end

---@param import_name string
---@return string
local function import_category(import_name)
    return (vim.startswith(import_name, "java.") or vim.startswith(import_name, "javax.")) and "java" or "other"
end

--- Insert one import while preserving the configured non-Java / Java / static grouping.
---@param bufnr integer
---@param import_name string
---@return integer inserted_line_count
local function insert_import(bufnr, import_name)
    local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
    local statement = "import " .. import_name .. ";"
    local category = import_category(import_name)
    local regular_imports = {}
    local package_index = nil
    local first_static_index = nil

    for index, line in ipairs(lines) do
        if line == statement then
            return 0
        end
        if line:match("^%s*package%s+") then
            package_index = index
        elseif line:match("^%s*import%s+static%s+") then
            first_static_index = first_static_index or index
        else
            local existing = line:match("^%s*import%s+([%w_%.%*]+)%s*;")
            if existing then
                regular_imports[#regular_imports + 1] = {
                    index = index,
                    name = existing,
                    category = import_category(existing),
                }
            end
        end
    end

    local same_category = {}
    local other_category = {}
    for _, item in ipairs(regular_imports) do
        local target = item.category == category and same_category or other_category
        target[#target + 1] = item
    end

    if #same_category > 0 then
        local insert_row = same_category[#same_category].index
        for _, item in ipairs(same_category) do
            if import_name < item.name then
                insert_row = item.index - 1
                break
            end
        end
        vim.api.nvim_buf_set_lines(bufnr, insert_row, insert_row, false, { statement })
        return 1
    end

    if #other_category > 0 then
        if category == "other" then
            local first_java = other_category[1].index
            vim.api.nvim_buf_set_lines(bufnr, first_java - 1, first_java - 1, false, { statement, "" })
            return 2
        end

        local last_other = other_category[#other_category].index
        if lines[last_other + 1] == "" then
            local inserted = { statement }
            if lines[last_other + 2] ~= "" then
                inserted[#inserted + 1] = ""
            end
            vim.api.nvim_buf_set_lines(bufnr, last_other + 1, last_other + 1, false, inserted)
            return #inserted
        end

        vim.api.nvim_buf_set_lines(bufnr, last_other, last_other, false, { "", statement, "" })
        return 3
    end

    local insert_row = 0
    local inserted = { statement, "" }
    if first_static_index then
        insert_row = first_static_index - 1
        if lines[first_static_index - 1] == "" then
            insert_row = first_static_index - 1
        else
            table.insert(inserted, 1, "")
        end
    elseif package_index then
        if lines[package_index + 1] == "" then
            insert_row = package_index + 1
        else
            insert_row = package_index
            table.insert(inserted, 1, "")
        end
    end

    vim.api.nvim_buf_set_lines(bufnr, insert_row, insert_row, false, inserted)
    return #inserted
end

--- Apply planned imports and return how many lines were inserted before the mapper type.
---@param bufnr integer
---@param imports string[]
---@return integer inserted_line_count
function M.apply(bufnr, imports)
    table.sort(imports, function(left, right)
        local left_category = import_category(left)
        local right_category = import_category(right)
        if left_category ~= right_category then
            return left_category == "other"
        end
        return left < right
    end)

    local inserted = 0
    for _, import_name in ipairs(imports) do
        inserted = inserted + insert_import(bufnr, import_name)
    end
    return inserted
end

return M
