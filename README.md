# fabulous-minutes

A simple logging "plug-in" for the awesome [Fable.Remoting](https://github.com/Zaid-Ajaj/Fable.Remoting) library.

## Dev

Run all bash commands in the root folder.

#### Run tests

```bash
.\build.cmd runtests
```

#### Update project version and release notes

```bash
.\build.cmd RN [semver:patch || semver:minor || semver:major]
```

Use `.\build.cmd RN` to update current release with git commits. Add `semver:xxx` to increase project to the 
next chosen semantic version **and** add all new commits to that version.