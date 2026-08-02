-- Java file templates: package / class-name matchers -> LuaSnip trigger.
--
-- This is THE configuration point. Rules are evaluated top-to-bottom and the
-- first match wins. Within one rule every matcher field present must match
-- (logical AND); the globs listed inside a single field are alternatives (OR).
-- A rule with no matcher field at all is a catch-all.
--
-- Rule fields:
--   snippet     LuaSnip trigger, looked up in `java-template` then `java`
--   packages    globs matched case-insensitively against the dotted package
--   filename    globs matched against the class name (file name without `.java`)
--   path        globs matched against the absolute file path
--   source_set  globs matched against the source set (`main`, `test`, ...)
--   when        fun(ctx): boolean — arbitrary extra condition
--   choice      index preselected on the template's FIRST choice node
--   desc        label shown by the `:FileTemplate` picker
--
-- Glob syntax: `*` = any characters (dots included), `?` = exactly one character.
-- Patterns are anchored on BOTH ends, so `*` is needed on every open side and may
-- be used on either side, or both:
--
--   "*.listeners"      com.acme.listeners            ✓
--                      com.acme.kafka.listeners      ✓  (`*` spans dots)
--                      com.acme.listeners.internal   ✗  (anchored at the end)
--                      com.acme.mylisteners          ✗  (the dot is literal)
--                      listeners                     ✗  (no leading dot to match)
--   "*.listeners.*"    com.acme.listeners.internal   ✓  sub-packages only
--   "*.listeners*"     both of the above             ✓  plus com.acme.listenersX
--   "*listeners"       com.acme.mylisteners          ✓  suffix, ignores segments
--   "listeners"        listeners                     ✓  exact, top-level package
--   "*.a.b.listeners"  segments must be adjacent: com.a.b.listeners ✓, com.a.x.b.listeners ✗
--
-- List several alternatives instead of over-widening one glob:
--   packages = { "*.listeners", "*.listeners.*" }
--
-- `packages` and `source_set` are matched case-insensitively; `filename` and
-- `path` are case-sensitive.
--
-- `choice` indexes for the reused Spring/Lombok snippets:
--   component -> 1 @Component | 2 @Configuration | 3 @Service | 4 @Repository
--   mapper    -> 1 @Mapper(unmappedTargetPolicy = ERROR) | 2 @Mapper
--   immutable -> 1 @Value + @Builder | 2 @Value(staticConstructor = "of")
--   usecase   -> 1 implements UseCase<..> | 2 extends ..
--   exception -> 1 RuntimeException | 2 Exception | 3 custom parent

---@type file_template.Rule[]
return {
    -- ── class-name driven (strongest signal, wins over the package) ──────────
    -- `source_set` guards against a `*Test` named production class: the file must
    -- live in a test source set (`src/test/java`, `src/integrationTest/java`, ...),
    -- not in `src/main/java`.
    {
        snippet = "test",
        filename = { "*Test", "*Tests", "*TestCase", "*IT", "*ITCase" },
        source_set = { "*test*" },
        desc = "JUnit 5 test class",
    },
    { snippet = "exception", filename = { "*Exception", "*Error" }, desc = "Exception class" },
    { snippet = "controller", filename = { "*Controller", "*Resource", "*Endpoint" }, desc = "Spring @RestController" },
    { snippet = "mapper", filename = { "*Mapper" }, desc = "MapStruct @Mapper" },
    { snippet = "repository", filename = { "*Repository", "*Dao" }, desc = "Spring Data repository" },
    { snippet = "properties", filename = { "*Properties" }, desc = "Spring @ConfigurationProperties record" },
    { snippet = "component", filename = { "*Config", "*Configuration" }, choice = 2, desc = "Spring @Configuration" },
    { snippet = "component", filename = { "*Service", "*ServiceImpl" }, choice = 3, desc = "Spring @Service" },
    { snippet = "usecase", filename = { "*UseCase", "*Usecase", "*Interactor" }, desc = "Use case component" },
    { snippet = "gateway", filename = { "*Gateway" }, desc = "Gateway component" },
    { snippet = "util", filename = { "*Util", "*Utils", "*Helper", "*Helpers" }, desc = "Lombok @UtilityClass" },

    -- ── package driven ──────────────────────────────────────────────────────
    {
        snippet = "util",
        packages = { "*.util", "*.utils", "*.helper", "*.helpers", "*.support" },
        desc = "Lombok @UtilityClass",
    },
    {
        snippet = "usecase",
        packages = { "*.usecase", "*.usecases", "*.interaction", "*.interactions", "*.interactor", "*.interactors" },
        desc = "Use case component",
    },
    { snippet = "gateway", packages = { "*.gateway", "*.gateways" }, desc = "Gateway component" },
    {
        snippet = "controller",
        packages = {
            "*.controller",
            "*.controllers",
            "*.rest",
            "*.web",
            "*.api",
            "*.endpoint",
            "*.endpoints",
            "*.resource",
            "*.resources",
        },
        desc = "Spring @RestController",
    },
    {
        snippet = "mapper",
        packages = { "*.mapper", "*.mappers", "*.converter", "*.converters" },
        desc = "MapStruct @Mapper",
    },
    {
        snippet = "repository",
        packages = { "*.repository", "*.repositories", "*.dao" },
        desc = "Spring Data repository",
    },
    { snippet = "properties", packages = { "*.properties" }, desc = "Spring @ConfigurationProperties record" },
    {
        snippet = "component",
        packages = { "*.config", "*.configs", "*.configuration", "*.configurations" },
        choice = 2,
        desc = "Spring @Configuration",
    },
    { snippet = "component", packages = { "*.service", "*.services" }, choice = 3, desc = "Spring @Service" },
    {
        snippet = "component",
        packages = {
            "*.component",
            "*.components",
            "*.client",
            "*.clients",
            "*.adapter",
            "*.adapters",
            "*.listener",
            "*.listeners",
            "*.consumer",
            "*.consumers",
            "*.producer",
            "*.producers",
            "*.scheduler",
            "*.schedulers",
            "*.job",
            "*.jobs",
        },
        choice = 1,
        desc = "Spring @Component",
    },
    {
        snippet = "record",
        packages = {
            "*.dto",
            "*.dtos",
            "*.request",
            "*.requests",
            "*.response",
            "*.responses",
            "*.payload",
            "*.payloads",
            "*.command",
            "*.commands",
            "*.query",
            "*.queries",
            "*.event",
            "*.events",
        },
        desc = "Java record",
    },
    {
        snippet = "immutable",
        packages = { "*.model", "*.models", "*.domain", "*.value", "*.vo" },
        desc = "Lombok @Value + @Builder",
    },
    { snippet = "mutable", packages = { "*.entity", "*.entities" }, desc = "Lombok @Data + @Builder" },
    {
        snippet = "enum",
        packages = { "*.enum", "*.enums", "*.type", "*.types", "*.status", "*.statuses" },
        desc = "Enum",
    },
    {
        snippet = "interface",
        packages = { "*.spi", "*.port", "*.ports", "*.contract", "*.contracts", "*.facade", "*.facades" },
        desc = "Interface",
    },
    {
        snippet = "exception",
        packages = { "*.exception", "*.exceptions", "*.error", "*.errors" },
        desc = "Exception class",
    },

    -- ── catch-all ───────────────────────────────────────────────────────────
    { snippet = "class", desc = "Plain class" },
}
