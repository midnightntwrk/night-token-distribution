#!/bin/bash

# Define files
JSON_FILE="plutus.json"
TOML_FILE="aiken.toml"


# Define Bullet validator position and TOML keys
# Defined in order of dependencies
PROTOCOL_PARAMS_POLICY_KEY=".validators.[2].hash"
PROTOCOL_PARAMS_POLICY_TOML_KEY="protocol_params_policy"

# Check if network parameter is provided
if [ $# -lt 1 ]; then
    echo "Error: Network parameter required."
    echo "Usage: $0 <Mainnet|Preview|Preprod> [silent|verbose|compact]"
    exit 1
fi

# Convert parameter to lowercase for consistent comparison
NETWORK=$(echo "$1" | tr '[:upper:]' '[:lower:]')

TRACE_LEVEL=$([ $# -eq 2 ] && echo "-t $2" || echo "")


# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Check if required commands are available
if ! command_exists aiken; then
    echo "Error: aiken is not installed. Please install aiken first."
    exit 1
fi

if ! command_exists jq; then
    echo "Error: jq is not installed. Please install jq first."
    exit 1
fi

if ! command_exists toml; then
    echo "Error: toml-cli is not installed. Please install toml-cli first."
    exit 1
fi

# Compile code
aiken build --env $NETWORK $TRACE_LEVEL 2>/dev/null

# Check if JSON file exists
if [ ! -f "$JSON_FILE" ]; then
    echo "Error: JSON file '$JSON_FILE' not found."
    exit 1
fi

# Check if JSON file is readable
if [ ! -r "$JSON_FILE" ]; then
    echo "Error: Cannot read JSON file '$JSON_FILE'. Check permissions."
    exit 1
fi

# Read value from JSON file
JSON_VALUE=$(jq -r "$PROTOCOL_PARAMS_POLICY_KEY" "$JSON_FILE" 2>/dev/null)

# Check if jq command was successful and returned a value
if [ $? -ne 0 ] || [ "$JSON_VALUE" == "null" ]; then
    echo "Error: Failed to read value from JSON file or key not found."
    exit 1
fi

# Check if TOML file exists
if [ ! -f "$TOML_FILE" ]; then
    echo "Error: TOML file '$TOML_FILE' not found."
    exit 1
fi

# Check if TOML file is readable
if [ ! -r "$TOML_FILE" ]; then
    echo "Error: Cannot read TOML file '$TOML_FILE'. Check permissions."
    exit 1
fi

# Write value to TOML file
NEW_TOML_CONTENT=$(toml set "$TOML_FILE" "config.$NETWORK.$PROTOCOL_PARAMS_POLICY_TOML_KEY.bytes" "$JSON_VALUE" 2>/dev/null)
echo "$NEW_TOML_CONTENT" > $TOML_FILE

# Check if toml command was successful
if [ $? -ne 0 ]; then
    echo "Error: Failed to write value to TOML file."
    exit 1
fi

# Finally compile onchain ready code
aiken build --env $NETWORK $TRACE_LEVEL 2>/dev/null

# Check if aiken command was successful
if [ $? -ne 0 ]; then
    echo "Error: Failed to build aiken."
    exit 1
fi

echo "Successfully compiled Protocol Params."

exit 0
