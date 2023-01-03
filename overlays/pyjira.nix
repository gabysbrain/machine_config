python-final: python-prev:
{
  jira = python-prev.jira.overridePythonAttrs (old: {
    propagatedBuildInputs = with python-prev; [
      defusedxml
      keyring
      requests-oauthlib
      requests-toolbelt
      packaging
      typing-extensions
    ];
  });
}
