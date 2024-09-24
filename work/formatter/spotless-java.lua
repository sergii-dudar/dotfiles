return {
    meta = {
        url = "https://github.com/google/google-java-format",
        description = "Reformats Java source code according to Google Java Style.",
    },
    command = "mvn spotless:apply",
    args = { "-" },
}