# Config file at ~/.config/ladybug

- directory # Topic directory
- defaults # Default topic configuration
- default_topic # Default topic to use when not specified
- confirm = true # Ask for confirmation when performing destructive operations
- duplicates = "delete|keep|rename" # What to do when a file already exists

# Dotfiles at ~/ladybug

- Contains topics

# Topic config

- A topic is a directory containing ladybug.toml
- root = "~/"
- type = "shallow|deep" # Shallow topics may specify sub-topics, but can only contain surface-level
  files. Deep topics can contain directories, but not topics.
- template
- dependencies
- pre_hook
- post_hook
- private
- public

# Commands

- topic
  - add [--dry-run] [--root {root}] [--verbose] {topic}
    - Create a new topic within conf.directory using the default specified at conf.default
  - remove [--dry-run] [--verbose] {topic}
    - Remove an existing topic
  - edit {topic}
    - Open a topic config in $EDITOR
- add [--dry-run] [--topic {topic}] [--verbose] {glob}
  - Add files from the cwd to the specified topic or cwd
- deploy --dry-run [--topic {topic}] [--verbose]
  - Deploy the provided topic or all topics
- # mod --dry-run --topic --template --ignore {glob}
