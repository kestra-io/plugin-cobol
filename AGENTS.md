# Kestra Cobol Plugin

## What

- Provides plugin components under `io.kestra.plugin.cobol`.
- Includes classes such as `MessageOutput`, `CreateProgram`, `CallJob`, `NonInteractiveSignonHandler`.

## Why

- What user problem does this solve? Teams need to run, submit and create IBM i COBOL programs and jobs from orchestrated workflows instead of relying on manual console work, ad hoc scripts, or disconnected schedulers.
- Why would a team adopt this plugin in a workflow? It keeps IBM i / AS400 COBOL steps in the same Kestra flow as upstream preparation, approvals, retries, notifications, and downstream systems.
- What operational/business outcome does it enable? It reduces manual handoffs and fragmented tooling while improving reliability, traceability, and delivery speed for processes that depend on IBM i / AS400 COBOL.

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
