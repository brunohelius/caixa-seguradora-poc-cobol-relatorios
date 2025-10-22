# ProLeap COBOL Parser Setup

## Overview

This directory contains the ProLeap COBOL Parser setup for parsing COBOL source files. The parser has been installed, built, and configured to analyze COBOL files from the project.

## Installation Date

October 22, 2025

## Directory Structure

```
docs/parser/
├── README.md                          # This file
├── proleap-cobol-parser/              # ProLeap COBOL Parser repository
│   ├── src/                           # Parser source code
│   ├── target/                        # Compiled JAR and dependencies
│   │   ├── proleap-cobol-parser-4.0.0.jar
│   │   └── lib/                       # All runtime dependencies
│   └── pom.xml                        # Maven configuration
├── copybooks/                         # COBOL files to parse
│   ├── RG1866B.cbl                    # Original file (CRLF line endings)
│   └── RG1866B_unix.cbl               # Converted to Unix line endings
├── CobolParserRunner.java             # First parser attempt
├── SimpleCobolParser.java             # Simplified parser
├── DirectoryCobolParser.java          # Directory-based parser
├── FinalCobolParser.java              # Final parser implementation
└── parser-output.txt                  # Parser output (when successful)
```

## Components Installed

### 1. ProLeap COBOL Parser
- **Repository**: https://github.com/uwol/proleap-cobol-parser
- **Version**: 4.0.0
- **Build Tool**: Apache Maven 3.9.11
- **Java Version**: OpenJDK 21.0.8

### 2. Dependencies
All dependencies were successfully downloaded and installed to `proleap-cobol-parser/target/lib/`:
- antlr4-4.7.2.jar
- antlr-runtime-3.5.2.jar
- antlr4-runtime-4.7.2.jar
- ST4-4.1.jar
- org.abego.treelayout.core-1.0.3.jar
- javax.json-1.0.4.jar
- icu4j-61.1.jar
- slf4j-api-2.0.9.jar
- And test dependencies (JUnit, Logback, etc.)

## Build Process

The parser was built using Maven:

```bash
cd docs/parser/proleap-cobol-parser
mvn clean install -DskipTests
mvn dependency:copy-dependencies -DoutputDirectory=target/lib
```

Both commands completed successfully with BUILD SUCCESS status.

## Parser Programs

### FinalCobolParser.java

The main parser program that attempts to parse COBOL files using different format options:

**Features**:
- Tries FIXED, VARIABLE, and TANDEM formats
- Provides detailed error reporting
- Extracts compilation unit structure
- Analyzes all COBOL divisions (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Outputs full Abstract Syntax Tree (AST)

**Usage**:
```bash
cd docs/parser
javac -cp "proleap-cobol-parser/target/proleap-cobol-parser-4.0.0.jar:proleap-cobol-parser/target/lib/*" FinalCobolParser.java
java -cp ".:proleap-cobol-parser/target/proleap-cobol-parser-4.0.0.jar:proleap-cobol-parser/target/lib/*" FinalCobolParser
```

## Target COBOL File

**File**: RG1866B.cbl
**Location**: `/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/RG1866B.cbl`
**Size**: 237,116 bytes (original with CRLF), 232,070 bytes (Unix LF)
**Encoding**: Non-ISO extended-ASCII

**Program Details**:
- **Program ID**: RG1866B
- **System**: REGISTROS GERAIS
- **Function**: CIRCULAR SUSEP 360 - PREMIT.TXT E PREMCED.TXT (PREMIOS EMITIDOS)
- **Analyst**: GILSON
- **Programmer**: WELLINGTON F R C VERAS
- **Date**: 21/05/2014

## Known Issues

### Parser Error: "Input length = 1"

The parser encounters an error when attempting to preprocess the COBOL file:

```
io.proleap.cobol.preprocessor.impl.CobolPreprocessorImpl --
Preprocessing file RG1866B_unix.cbl with line format FIXED and charset UTF-8.
Failed: Input length = 1
```

This error occurs with all format types (FIXED, VARIABLE, TANDEM).

**Possible Causes**:
1. **Character Encoding**: The file uses Non-ISO extended-ASCII encoding, which may not be fully compatible with UTF-8 parsing
2. **COBOL Format Specifics**: The file may use a proprietary or system-specific COBOL format not recognized by the parser
3. **Special Characters**: The file may contain special characters or control characters that the preprocessor cannot handle
4. **Column Layout**: The COBOL column layout might not match the expected format for the parser

**Attempted Solutions**:
- ✓ Converted CRLF line endings to Unix LF
- ✓ Tried all available format types (FIXED, VARIABLE, TANDEM)
- ✗ Character encoding conversion (needs further investigation)

## Next Steps

To resolve the parsing issue:

1. **Investigate Character Encoding**:
   ```bash
   iconv -f WINDOWS-1252 -t UTF-8 RG1866B.cbl > RG1866B_utf8.cbl
   ```

2. **Verify COBOL Column Format**:
   - Columns 1-6: Sequence numbers
   - Column 7: Indicator area (*, -, D, /)
   - Columns 8-11: Area A
   - Columns 12-72: Area B

3. **Try Alternative Parsers**:
   - GnuCOBOL (cobc)
   - COBOL-IT
   - IBM COBOL parser

4. **Manual Code Analysis**:
   - If parsing fails, perform manual code review and documentation

## References

- **ProLeap COBOL Parser GitHub**: https://github.com/uwol/proleap-cobol-parser
- **ANTLR**: https://www.antlr.org/
- **COBOL Standards**: ANSI X3.23-1985, ISO/IEC 1989:2014

## Support

For issues with the ProLeap COBOL Parser, refer to:
- GitHub Issues: https://github.com/uwol/proleap-cobol-parser/issues
- Documentation: Project README and source code comments

---

**Setup completed by**: Claude Code
**Date**: October 22, 2025
