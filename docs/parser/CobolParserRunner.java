import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;

public class CobolParserRunner {

    public static void main(String[] args) {
        try {
            if (args.length < 1) {
                System.err.println("Usage: java CobolParserRunner <cobol-file-path>");
                System.exit(1);
            }

            String cobolFilePath = args[0];
            File inputFile = new File(cobolFilePath);

            if (!inputFile.exists()) {
                System.err.println("File not found: " + cobolFilePath);
                System.exit(1);
            }

            System.out.println("Parsing COBOL file: " + cobolFilePath);

            // Create output file
            String outputPath = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/parser-output.txt";
            File outputFile = new File(outputPath);
            PrintWriter writer = new PrintWriter(new FileWriter(outputFile));

            // Initialize parser
            CobolParserRunnerImpl runner = new CobolParserRunnerImpl();

            // Try different format enums
            Program program = null;
            CobolSourceFormatEnum[] formats = {
                CobolSourceFormatEnum.FIXED,
                CobolSourceFormatEnum.TANDEM,
                CobolSourceFormatEnum.VARIABLE
            };

            Exception lastException = null;
            for (CobolSourceFormatEnum format : formats) {
                try {
                    System.out.println("Trying format: " + format);
                    program = runner.analyzeFile(inputFile, format);
                    System.out.println("Successfully parsed with format: " + format);
                    writer.println("Format used: " + format);
                    break;
                } catch (Exception e) {
                    lastException = e;
                    System.out.println("Failed with format " + format + ": " + e.getMessage());
                }
            }

            if (program == null) {
                throw new Exception("Failed to parse with any format. Last error: " +
                    (lastException != null ? lastException.getMessage() : "Unknown"));
            }

            writer.println("=== COBOL PARSER OUTPUT ===");
            writer.println("File: " + cobolFilePath);
            writer.println("Date: " + new java.util.Date());
            writer.println();

            // Get compilation units
            writer.println("Number of compilation units: " + program.getCompilationUnits().size());
            writer.println();

            for (CompilationUnit compilationUnit : program.getCompilationUnits()) {
                writer.println("=== COMPILATION UNIT ===");
                writer.println("Name: " + compilationUnit.getName());
                writer.println();

                // Get program unit
                ProgramUnit programUnit = compilationUnit.getProgramUnit();
                if (programUnit != null) {
                    writer.println("Program Unit Type: " + programUnit.getClass().getSimpleName());
                    writer.println();

                    // Data Division
                    DataDivision dataDivision = programUnit.getDataDivision();
                    if (dataDivision != null) {
                        writer.println("--- DATA DIVISION ---");
                        writer.println("Data Division found: Yes");

                        if (dataDivision.getWorkingStorageSection() != null) {
                            writer.println("Working Storage Section: Yes");
                        }
                        if (dataDivision.getFileSection() != null) {
                            writer.println("File Section: Yes");
                        }
                        if (dataDivision.getLinkageSection() != null) {
                            writer.println("Linkage Section: Yes");
                        }
                        writer.println();
                    }

                    // Procedure Division
                    ProcedureDivision procedureDivision = programUnit.getProcedureDivision();
                    if (procedureDivision != null) {
                        writer.println("--- PROCEDURE DIVISION ---");
                        writer.println("Procedure Division found: Yes");

                        if (procedureDivision.getParagraphs() != null) {
                            writer.println("Number of Paragraphs: " + procedureDivision.getParagraphs().size());
                        }
                        if (procedureDivision.getSections() != null) {
                            writer.println("Number of Sections: " + procedureDivision.getSections().size());
                        }
                        writer.println();
                    }
                }
            }

            // Print AST as string representation
            writer.println("=== ABSTRACT SYNTAX TREE ===");
            writer.println(program.toString());
            writer.println();

            writer.println("=== END OF PARSER OUTPUT ===");
            writer.close();

            System.out.println("Parsing completed successfully!");
            System.out.println("Output saved to: " + outputFile.getAbsolutePath());

            // Also print summary to console
            System.out.println("\n=== SUMMARY ===");
            System.out.println("Compilation units: " + program.getCompilationUnits().size());
            System.out.println("Output file: " + outputFile.getAbsolutePath());

        } catch (Exception e) {
            System.err.println("Error parsing COBOL file: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}
