import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class FinalCobolParser {

    public static void main(String[] args) {
        PrintWriter writer = null;
        try {
            System.out.println("=== ProLeap COBOL Parser ===");
            System.out.println();

            // Create output file
            String outputPath = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/parser-output.txt";
            writer = new PrintWriter(new FileWriter(outputPath));

            writer.println("=== COBOL PARSER OUTPUT ===");
            writer.println("Generated: " + new java.util.Date());
            writer.println();

            // COBOL file to parse (using Unix line endings version)
            File cobolFile = new File("/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/copybooks/RG1866B_unix.cbl");

            System.out.println("Input file: " + cobolFile.getAbsolutePath());
            System.out.println("File exists: " + cobolFile.exists());
            System.out.println("File size: " + cobolFile.length() + " bytes");
            System.out.println();

            writer.println("Input file: " + cobolFile.getName());
            writer.println("File size: " + cobolFile.length() + " bytes");
            writer.println();

            // Initialize parser
            CobolParserRunnerImpl runner = new CobolParserRunnerImpl();
            Program program = null;

            // Try parsing with different formats
            CobolSourceFormatEnum[] formats = {
                CobolSourceFormatEnum.FIXED,
                CobolSourceFormatEnum.VARIABLE,
                CobolSourceFormatEnum.TANDEM
            };

            boolean parsed = false;
            String successFormat = "";

            for (CobolSourceFormatEnum format : formats) {
                try {
                    System.out.println("Trying format: " + format);
                    program = runner.analyzeFile(cobolFile, format);

                    // Check if parsing was successful by verifying we have compilation units
                    if (program != null && program.getCompilationUnits().size() > 0) {
                        System.out.println("SUCCESS with format: " + format);
                        successFormat = format.toString();
                        parsed = true;
                        break;
                    }
                } catch (Exception e) {
                    System.out.println("  Failed: " + e.getMessage());
                }
            }

            if (!parsed || program == null) {
                writer.println("ERROR: Could not parse the COBOL file with any combination of format and charset");
                writer.println("This may indicate:");
                writer.println("  1. The file is not in a standard COBOL format");
                writer.println("  2. The file encoding is not supported");
                writer.println("  3. The file has syntax errors");
                writer.close();
                System.err.println("ERROR: Failed to parse the COBOL file");
                System.exit(1);
            }

            // Parsing successful
            writer.println("Parsing successful!");
            writer.println("Format: " + successFormat);
            writer.println();

            // Get compilation units
            int unitCount = program.getCompilationUnits().size();
            writer.println("Compilation units found: " + unitCount);
            writer.println();

            System.out.println("\nAnalyzing " + unitCount + " compilation unit(s)...\n");

            // Analyze each compilation unit
            int unitIndex = 1;
            for (CompilationUnit unit : program.getCompilationUnits()) {
                writer.println("========================================");
                writer.println("COMPILATION UNIT #" + unitIndex);
                writer.println("========================================");
                writer.println();
                writer.println("Name: " + unit.getName());
                writer.println();

                if (unit.getProgramUnit() != null) {
                    writer.println("PROGRAM STRUCTURE:");
                    writer.println();

                    // Identification Division
                    if (unit.getProgramUnit().getIdentificationDivision() != null) {
                        writer.println("✓ IDENTIFICATION DIVISION");
                        System.out.println("  - Found IDENTIFICATION DIVISION");
                    }

                    // Environment Division
                    if (unit.getProgramUnit().getEnvironmentDivision() != null) {
                        writer.println("✓ ENVIRONMENT DIVISION");
                        System.out.println("  - Found ENVIRONMENT DIVISION");
                    }

                    // Data Division
                    if (unit.getProgramUnit().getDataDivision() != null) {
                        writer.println("✓ DATA DIVISION");
                        System.out.println("  - Found DATA DIVISION");

                        var dataDiv = unit.getProgramUnit().getDataDivision();

                        if (dataDiv.getFileSection() != null) {
                            int fileCount = dataDiv.getFileSection().getFileDescriptionEntries().size();
                            writer.println("  - File Section (Files: " + fileCount + ")");
                            System.out.println("    * File Section with " + fileCount + " files");
                        }

                        if (dataDiv.getWorkingStorageSection() != null) {
                            int dataItems = dataDiv.getWorkingStorageSection().getDataDescriptionEntries().size();
                            writer.println("  - Working Storage Section (Data items: " + dataItems + ")");
                            System.out.println("    * Working Storage Section with " + dataItems + " data items");
                        }

                        if (dataDiv.getLinkageSection() != null) {
                            writer.println("  - Linkage Section");
                            System.out.println("    * Linkage Section found");
                        }
                    }

                    // Procedure Division
                    if (unit.getProgramUnit().getProcedureDivision() != null) {
                        writer.println("✓ PROCEDURE DIVISION");
                        System.out.println("  - Found PROCEDURE DIVISION");

                        var procDiv = unit.getProgramUnit().getProcedureDivision();

                        if (procDiv.getParagraphs() != null) {
                            int paragraphs = procDiv.getParagraphs().size();
                            writer.println("  - Paragraphs: " + paragraphs);
                            System.out.println("    * " + paragraphs + " paragraphs");
                        }

                        if (procDiv.getSections() != null) {
                            int sections = procDiv.getSections().size();
                            writer.println("  - Sections: " + sections);
                            System.out.println("    * " + sections + " sections");
                        }
                    }

                    writer.println();
                }

                unitIndex++;
            }

            // Print full AST
            writer.println();
            writer.println("========================================");
            writer.println("ABSTRACT SYNTAX TREE (Full Structure)");
            writer.println("========================================");
            writer.println();
            writer.println(program.toString());
            writer.println();

            writer.println("=== END OF PARSER OUTPUT ===");
            writer.close();

            System.out.println();
            System.out.println("========================================");
            System.out.println("PARSING COMPLETE!");
            System.out.println("========================================");
            System.out.println("Output file: " + outputPath);
            System.out.println("Format used: " + successFormat);
            System.out.println("Compilation units: " + unitCount);
            System.out.println();

        } catch (Exception e) {
            System.err.println("FATAL ERROR: " + e.getMessage());
            e.printStackTrace();
            if (writer != null) {
                writer.println("FATAL ERROR: " + e.getMessage());
                e.printStackTrace(writer);
                writer.close();
            }
            System.exit(1);
        }
    }
}
