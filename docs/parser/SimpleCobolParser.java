import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Paths;

public class SimpleCobolParser {

    public static void main(String[] args) {
        try {
            if (args.length < 1) {
                System.err.println("Usage: java SimpleCobolParser <cobol-file-path>");
                System.exit(1);
            }

            String cobolFilePath = args[0];
            File inputFile = new File(cobolFilePath);

            if (!inputFile.exists()) {
                System.err.println("File not found: " + cobolFilePath);
                System.exit(1);
            }

            System.out.println("=== ProLeap COBOL Parser ===");
            System.out.println("File: " + cobolFilePath);
            System.out.println("File size: " + inputFile.length() + " bytes");
            System.out.println();

            // Create output file
            String outputPath = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/parser-output.txt";
            PrintWriter writer = new PrintWriter(new FileWriter(outputPath));

            writer.println("=== COBOL PARSER OUTPUT ===");
            writer.println("File: " + cobolFilePath);
            writer.println("File size: " + inputFile.length() + " bytes");
            writer.println("Date: " + new java.util.Date());
            writer.println();

            // Initialize parser
            CobolParserRunnerImpl runner = new CobolParserRunnerImpl();
            Program program = null;

            // Try FIXED format (standard COBOL format)
            System.out.println("Attempting to parse with FIXED format (standard COBOL)...");
            try {
                // Parse with FIXED format, which is the most common for traditional COBOL
                program = runner.analyzeFile(inputFile, CobolSourceFormatEnum.FIXED);
                System.out.println("SUCCESS: File parsed with FIXED format");
                writer.println("Format: FIXED");
                writer.println();
            } catch (Exception e) {
                System.err.println("FAILED with FIXED format: " + e.getMessage());
                e.printStackTrace();
                writer.println("ERROR: Failed to parse file");
                writer.println("Exception: " + e.getMessage());
                writer.println();

                // Print stack trace to output file
                writer.println("Stack trace:");
                e.printStackTrace(writer);
                writer.close();
                System.exit(1);
            }

            // Analysis successful, print results
            writer.println("=== PARSING SUCCESSFUL ===");
            writer.println();

            // Get compilation units
            int unitCount = program.getCompilationUnits().size();
            writer.println("Number of compilation units: " + unitCount);
            System.out.println("Number of compilation units: " + unitCount);
            writer.println();

            // Analyze each compilation unit
            for (CompilationUnit unit : program.getCompilationUnits()) {
                writer.println("--- COMPILATION UNIT ---");
                writer.println("Name: " + unit.getName());

                if (unit.getProgramUnit() != null) {
                    writer.println("Program Unit: Yes");

                    if (unit.getProgramUnit().getIdentificationDivision() != null) {
                        writer.println("  - Identification Division: Yes");
                    }
                    if (unit.getProgramUnit().getEnvironmentDivision() != null) {
                        writer.println("  - Environment Division: Yes");
                    }
                    if (unit.getProgramUnit().getDataDivision() != null) {
                        writer.println("  - Data Division: Yes");
                    }
                    if (unit.getProgramUnit().getProcedureDivision() != null) {
                        writer.println("  - Procedure Division: Yes");
                    }
                }
                writer.println();
            }

            // Print program structure as string
            writer.println("=== PROGRAM STRUCTURE (toString) ===");
            writer.println(program.toString());
            writer.println();

            writer.println("=== END OF PARSER OUTPUT ===");
            writer.close();

            System.out.println();
            System.out.println("=== PARSING COMPLETE ===");
            System.out.println("Output saved to: " + outputPath);
            System.out.println();
            System.out.println("Summary:");
            System.out.println("  - Compilation units: " + unitCount);
            System.out.println("  - Format: FIXED (standard COBOL)");

        } catch (Exception e) {
            System.err.println("FATAL ERROR: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
