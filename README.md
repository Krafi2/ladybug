# Ladybug

Ladybug is a dotfile manager that allows you to organize your dotfiles into topics and specify
dependencies among them.

## Installing

You can install ladybug using cargo.

```
cargo install --git https://github.com/Krafi2/ladybug.git
```

## Usage

Deploy your dotfiles with `ladybug deploy`.

## Configuration

The global configuration file is located at `$XDG_CONFIG_HOME/ladybug/ladybug.toml`, which is
`~/.config/ladybug/ladybug.toml` on most systems.

| Option        | Value           | Default                 | Description                                                            |
| ------------- | --------------- | ----------------------- | ---------------------------------------------------------------------- |
| dotfile_dir   | string          | "~/ladybug"             | The location of the dotfile directory.                                 |
| topic.default | dict            | empty                   | Default configuration options for every topic.                         |
| shell         | list of strings | ["sh", "-c", "{{cmd}}"] | The command used for running hooks. `{{cmd}}` is replaced by the hook. |

Individual topics are configured using `ladybug.toml` files in their directories.

| Option      | Value                                                                      | Default           | Description                                                                                                                                                                       |
| ----------- | -------------------------------------------------------------------------- | ----------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| target      | string                                                                     | \$XDG_CONFIG_HOME | The topic will be deployed to this directory.                                                                                                                                     |
| type        | "shallow"\|"deep"                                                          | "deep"            | The topic's type. Shallow topics can specify sub-topics, but can only contain surface-level files. Deep topics can contain directories, but not topics.                           |
| link        | list of strings                                                            | ["**"]            | A list of globs that specify which files will be linked to the target directory.                                                                                                  |
| template    | list of strings                                                            | []                | A list of globs that specify which files will be templated and copied to the target directory.                                                                                    |
| deps        | list of string                                                             | []                | A list of globs that specify the topic's dependencies. Globs beginning with `/` will be considered relative to the dotfile directory, while others will be relative to the topic. |
| hook.pre    | string \| {type = "file", file: string} \| {type = "command", cmd: string} | empty             | Hook to run in the target directory before deploying.                                                                                                                             |
| hook.post   | string \| {type = "file", file: string} \| {type = "command", cmd: string} | empty             | Hook to run in the target directory after deploying.                                                                                                                              |
| env.private | dict                                                                       | empty             | Modify the tera environment before templating.                                                                                                                                    |
| env.public  | dict                                                                       | empty             | Modify the tera environment before templating. Exported to topics that depend on this one.                                                                                        |
