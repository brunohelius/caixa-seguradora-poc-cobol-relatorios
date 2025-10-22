#!/bin/bash

# ProLeap COBOL Parser Runner Script
# This script compiles and runs the COBOL parser on the target file

set -e  # Exit on error

echo "======================================"
echo "ProLeap COBOL Parser"
echo "======================================"
echo ""

# Change to the parser directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

# Check if Java is installed
if ! command -v java &> /dev/null; then
    echo "ERROR: Java is not installed or not in PATH"
    exit 1
fi

# Check if Maven is installed
if ! command -v mvn &> /dev/null; then
    echo "WARNING: Maven is not installed or not in PATH"
    echo "This is only required if you need to rebuild the parser"
fi

echo "Java version:"
java -version
echo ""

# Set classpath
PARSER_JAR="proleap-cobol-parser/target/proleap-cobol-parser-4.0.0.jar"
LIB_DIR="proleap-cobol-parser/target/lib"
CLASSPATH=".:${PARSER_JAR}:${LIB_DIR}/*"

# Check if parser JAR exists
if [ ! -f "$PARSER_JAR" ]; then
    echo "ERROR: Parser JAR not found at $PARSER_JAR"
    echo "Please run 'mvn clean install -DskipTests' in the proleap-cobol-parser directory"
    exit 1
fi

# Check if dependencies exist
if [ ! -d "$LIB_DIR" ]; then
    echo "ERROR: Dependencies directory not found at $LIB_DIR"
    echo "Please run 'mvn dependency:copy-dependencies -DoutputDirectory=target/lib' in the proleap-cobol-parser directory"
    exit 1
fi

# Compile the parser program
echo "Compiling FinalCobolParser..."
javac -cp "$CLASSPATH" FinalCobolParser.java

if [ $? -ne 0 ]; then
    echo "ERROR: Compilation failed"
    exit 1
fi

echo "Compilation successful!"
echo ""

# Run the parser
echo "Running parser..."
echo ""
java -cp "$CLASSPATH" FinalCobolParser

echo ""
echo "======================================"
echo "Parser execution completed"
echo "======================================"
echo ""
echo "Check parser-output.txt for results"
