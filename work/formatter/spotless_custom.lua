---@type conform.FileFormatterConfig
return {
  --meta = {
  --  url = "https://github.com/google/google-java-format",
  --  description = "Reformats Java source code according to Google Java Style.",
  --},
  --command = "google-java-format",

  command = "/home/serhii/dotfiles/work/formatter/spotless.sh",
  args = { "-" },
  --args = {
  --  "$FILENAME",
  --},
  stdin = false,
}
