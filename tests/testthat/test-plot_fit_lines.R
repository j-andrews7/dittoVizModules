# Tests for .safe_build_model(), which turns a user-typed model formula into a
# fitted model for the scatter plot's custom fit lines. It is a sandbox: the
# formula text reaches stats::as.formula() and then a backend's fit function,
# which evaluates it.

test_that(".safe_build_model returns NULL for empty input", {
    # `|` instead of `||` here used to make a NULL formula a zero-length
    # condition and an "argument is of length zero" error, against the
    # documented NULL return.
    expect_null(.safe_build_model(NULL, iris, "lm"))
    expect_null(.safe_build_model("", iris, "lm"))
    expect_null(.safe_build_model("   ", iris, "lm"))
    expect_null(.safe_build_model("Sepal.Length ~ Sepal.Width", NULL, "lm"))
})


test_that(".safe_build_model builds a model from an allowed formula", {
    m <- .safe_build_model("Sepal.Length ~ Sepal.Width", iris, "lm")
    expect_s3_class(m, "lm")

    # Transforms on the right-hand side are part of the vocabulary.
    expect_s3_class(.safe_build_model("Sepal.Length ~ log(Sepal.Width)", iris, "lm"), "lm")
    expect_s3_class(.safe_build_model("Sepal.Length ~ poly(Sepal.Width, 2)", iris, "lm"), "lm")
})


test_that(".safe_build_model rejects a formula naming unknown columns", {
    expect_warning(
        expect_null(.safe_build_model("Sepal.Length ~ nope", iris, "lm")),
        "disallowed terms"
    )
})


test_that(".safe_build_model rejects input that is not a single formula", {
    expect_warning(
        expect_null(.safe_build_model("Sepal.Length ~ Sepal.Width; 1", iris, "lm")),
        "single formula"
    )
    expect_warning(
        expect_null(.safe_build_model("Sepal.Length + Sepal.Width", iris, "lm")),
        "must be a model formula"
    )
    expect_warning(
        expect_null(.safe_build_model("Sepal.Length ~~~ ", iris, "lm")),
        "Could not parse"
    )
})


test_that(".safe_build_model rejects a call in function position", {
    # This function used to walk the AST with its own copy of the allowlist,
    # which flattened `node[[1]]` with as.character(). A namespaced call gave a
    # length-3 vector, so `if (!fn %in% allowed)` raised "the condition has
    # length > 1" and the error escaped into the plot render instead of being
    # reported as a disallowed term. `log()(x)` flattened to "log", matched the
    # allowlist, and was accepted outright.
    for (f in c(
        "Sepal.Length ~ base::log(Sepal.Width)",
        "Sepal.Length ~ (log)(Sepal.Width)",
        "Sepal.Length ~ log()(Sepal.Width)",
        "Sepal.Length ~ I(system)('id')"
    )) {
        expect_warning(res <- .safe_build_model(f, iris, "lm"), "disallowed terms",
            info = f
        )
        expect_null(res, info = f)
    }
})


test_that(".safe_build_model rejects impure calls", {
    for (f in c(
        "Sepal.Length ~ system('id')",
        "Sepal.Length ~ eval(parse(text = 'Sepal.Width'))",
        "Sepal.Length ~ get('Sepal.Width')",
        "Sepal.Length ~ iris$Sepal.Width"
    )) {
        expect_warning(res <- .safe_build_model(f, iris, "lm"), "disallowed terms",
            info = f
        )
        expect_null(res, info = f)
    }
})


test_that(".safe_build_model rejects an unregistered backend", {
    expect_warning(
        expect_null(.safe_build_model("Sepal.Length ~ Sepal.Width", iris, "not_a_backend")),
        "Unrecognized model type"
    )
})
