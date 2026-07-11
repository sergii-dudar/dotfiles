local helper = require("tests.utils.spec_helper")

describe("utils.java.gf-includeexpr", function()
    local includeexpr

    before_each(function()
        helper.reset_vim()
        includeexpr = helper.reload("utils.java.gf-includeexpr")
    end)

    it("preserves resource paths with dotted file names", function()
        local path = "test/api/http/request/payment-authz-process-request.json"

        assert.are.equal(path, includeexpr.transform(path))
    end)

    it("preserves plain resource files by known extension", function()
        assert.are.equal("application-test.yml", includeexpr.transform("application-test.yml"))
        assert.are.equal("logback-test.xml", includeexpr.transform("logback-test.xml"))
    end)

    it("converts java package names to paths", function()
        assert.are.equal(
            "ua/raiffeisen/paymentauthz/PaymentAuthzController",
            includeexpr.transform("ua.raiffeisen.paymentauthz.PaymentAuthzController")
        )
    end)
end)