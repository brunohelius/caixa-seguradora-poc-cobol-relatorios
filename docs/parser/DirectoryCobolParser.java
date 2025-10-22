import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class DirectoryCobolParser {

    public static void main(String[] args) {
        try {
            System.out.println("=== ProLeap COBOL Directory Parser ===");

            // Create output file
            String outputPath = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/parser-output.txt";
            PrintWriter writer = new PrintWriter(new FileWriter(outputPath));

            writer.println("=== COBOL PARSER OUTPUT ===");
            writer.println("Date: " + new java.util.Date());
            writer.println();

            // Initialize parser
            CobolParserRunnerImpl runner = new CobolParserRunnerImpl();

            // Parse directory with COBOL files
            File cobolDir = new File("/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/copybooks");
            System.out.println("Parsing directory: " + cobolDir.getAbsolutePath());
            writer.println("Source directory: " + cobolDir.getAbsolutePath());
            writer.println();

            try {
                // Parse the directory with FIXED format (standard COBOL)
                Program program = runner.analyzeDirectory(cobolDir, CobolSourceFormatEnum.FIXED);

                System.out.println("SUCCESS: Directory parsed with FIXED format");
                writer.println("Format: FIXED (standard COBOL)");
                writer.println();

                // Get compilation units
                int unitCount = program.getCompilationUnits().size();
                writer.println("Number of compilation units: " + unitCount);
                System.out.println("Number of compilation units: " + unitCount);
                writer.println();

                // Analyze each compilation unit
                for (CompilationUnit unit : program.getCompilationUnits()) {
                    writer.println("========================================");
                    writer.println("COMPILATION UNIT: " + unit.getName());
                    writer.println("========================================");
                    writer.println();

                    if (unit.getProgramUnit() != null) {
                        writer.println("Program Unit Found: Yes");
                        writer.println();

                        // Identification Division
                        if (unit.getProgramUnit().getIdentificationDivision() != null) {
                            writer.println("IDENTIFICATION DIVISION: Yes");
                            var idDiv = unit.getProgramUnit().getIdentificationDivision();
                            if (idDiv.getProgramIdParagraph() != null) {
                                writer.println("  Program ID: " + idDiv.getProgramIdParagraph());
                            }
                            writer.println();
                        }

                        // Environment Division
                        if (unit.getProgramUnit().getEnvironmentDivision() != null) {
                            writer.println("ENVIRONMENT DIVISION: Yes");
                            writer.println();
                        }

                        // Data Division
                        if (unit.getProgramUnit().getDataDivision() != null) {
                            writer.println("DATA DIVISION: Yes");
                            var dataDiv = unit.getProgramUnit().getDataDivision();

                            if (dataDiv.getFileSection() != null) {
                                writer.println("  - File Section: Yes");
                                writer.println("    Files: " + dataDiv.getFileSection().getFileDescriptionEntries().size());
                            }

                            if (dataDiv.getWorkingStorageSection() != null) {
                                writer.println("  - Working Storage Section: Yes");
                                writer.println("    Data items: " + dataDiv.getWorkingStorageSection().getDataDescriptionEntries().size());
                            }

                            if (dataDiv.getLinkageSection() != null) {
                                writer.println("  - Linkage Section: Yes");
                            }
                            writer.println();
                        }

                        // Procedure Division
                        if (unit.getProgramUnit().getProcedureDivision() != null) {
                            writer.println("PROCEDURE DIVISION: Yes");
                            var procDiv = unit.getProgramUnit().getProcedureDivision();

                            if (procDiv.getParagraphs() != null) {
                                writer.println("  Paragraphs: " + procDiv.getParagraphs().size());
                            }
                            if (procDiv.getSections() != null) {
                                writer.println("  Sections: " + procDiv.getSections().size());
                            }
                            writer.println();
                        }
                    }
                }

                // Print program structure
                writer.println();
                writer.println("========================================");
                writer.println("FULL PROGRAM STRUCTURE");
                writer.println("========================================");
                writer.println();
                writer.println(program.toString());
                writer.println();

                writer.println("=== PARSING SUCCESSFUL ===");
                writer.close();

                System.out.println();
                System.out.println("=== PARSING COMPLETE ===");
                System.out.println("Output saved to: " + outputPath);
                System.out.println();
                System.out.println("Summary:");
                System.out.println("  - Compilation units: " + unitCount);
                System.out.println("  - Format: FIXED (standard COBOL)");
                System.out.println("  - Output file: " + outputPath);

            } catch (Exception e) {
                System.err.println("ERROR: Failed to parse COBOL files");
                System.err.println("Exception: " + e.getMessage());
                e.printStackTrace();

                writer.println("ERROR: Failed to parse COBOL files");
                writer.println("Exception: " + e.getMessage());
                writer.println();
                writer.println("Stack trace:");
                e.printStackTrace(writer);
                writer.close();
                System.exit(1);
            }

        } catch (Exception e) {
            System.err.println("FATAL ERROR: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
