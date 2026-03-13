# Kestra Cobol Plugin

## What

description = 'Cobol Plugin for Kestra Exposes 3 plugin components (tasks, triggers, and/or conditions).

## Why

Enables Kestra workflows to interact with Cobol, allowing orchestration of Cobol-based operations as part of data pipelines and automation workflows.

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

### Important Commands

```bash
# Build the plugin
./gradlew shadowJar

# Run tests
./gradlew test

# Build without tests
./gradlew shadowJar -x test
```

### Configuration

All tasks and triggers accept standard Kestra plugin properties. Credentials should use
`{{ secret('SECRET_NAME') }}` — never hardcode real values.

## Agents

**IMPORTANT:** This is a Kestra plugin repository (prefixed by `plugin-`, `storage-`, or `secret-`). You **MUST** delegate all coding tasks to the `kestra-plugin-developer` agent. Do NOT implement code changes directly — always use this agent.
