# Kestra Cobol Plugin

## What

- Provides plugin components under `io.kestra.plugin.cobol`.
- Includes classes such as `MessageOutput`, `CreateProgram`, `CallJob`, `NonInteractiveSignonHandler`.

## Why

- This plugin integrates Kestra with IBM i / AS400 COBOL.
- It provides tasks that run, submit and create IBM i COBOL programs and jobs.

## How

### Architecture

Single-module plugin. Source packages under `io.kestra.plugin`:

- `cobol`

Infrastructure dependencies (Docker Compose services):

- `mainframe-sim`

### Key Plugin Classes

- `io.kestra.plugin.cobol.CallJob`
- `io.kestra.plugin.cobol.CreateProgram`
- `io.kestra.plugin.cobol.SubmitJob`

### Project Structure

```
plugin-cobol/
├── src/main/java/io/kestra/plugin/cobol/
├── src/test/java/io/kestra/plugin/cobol/
├── build.gradle
└── README.md
```

## References

- https://kestra.io/docs/plugin-developer-guide
- https://kestra.io/docs/plugin-developer-guide/contribution-guidelines
