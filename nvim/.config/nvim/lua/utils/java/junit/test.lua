-- local nio = require("nio")
-- nio.run(function()
--     local tree = require("neotest-java.core.positions_discoverer")({
--         method_id_resolver = {
--             resolve_complete_method_id = function(_, method_id)
--                 return method_id
--             end,
--         },
--     }).discover_positions(
--         "/home/serhii/serhii.home/git/tests/demo-spring/src/test/java/com/example/demo/DemoApplicationTest.java"
--     )
--     -- dd(tree:to_list())
--     -- dd(tree:data())
--     -- dd(tree:children())
--     -- dd(tree:get_key("com.example.demo.DemoApplicationTest"))
--     dd(tree:get_key("com.example.demo.DemoApplicationTest#contextLoads2()"))
-- end)
local nio = require("nio")
local MethodIdResolver = require("neotest-java.method_id_resolver")
local CommandExecutor = require("neotest-java.command.command_executor")
nio.run(function()
    local tree = require("neotest-java.core.positions_discoverer")({
        method_id_resolver = MethodIdResolver({
            classpath_provider = classpath_provider,
            command_executor = CommandExecutor(),
            binaries = binaries,
        }),
    }).discover_positionsiiii(
        "/home/serhii/serhii.home/git/tests/demo-spring/src/test/java/com/example/demo/DemoApplicationTest.java"
    )
    -- dd(tree:to_list())
    -- dd(tree:data())
    -- dd(tree:children())
    -- dd(tree:get_key("com.example.demo.DemoApplicationTest"))
    -- dd(tree:get_key("com.example.demo.DemoApplicationTest"))
    -- dd(tree:get_key("com.example.demo.DemoApplicationTest#contextLoads2()"))

    -- dd(tree:get_key("com.example.demo.DemoApplicationTest#contextLoads()"))
    -- dd(tree:get_key("com.example.demo.DemoApplicationTest#contextLoads2()"))
    dd(tree)
end)