#!/usr/bin/env bash
set -euo pipefail

COMPOSE_FILE="docker-compose-ci.yml"

docker compose -f "${COMPOSE_FILE}" down -v --remove-orphans
docker compose -f "${COMPOSE_FILE}" up --quiet-pull -d --wait

echo "Mainframe simulator is up on localhost ports 8470-8476."
echo "Run integration tests with: COBOL_INTEGRATION_TESTS=true ./gradlew test"
