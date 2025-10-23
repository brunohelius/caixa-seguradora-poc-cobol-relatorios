import io.proleap.cobol.asg.metamodel.CompilationUnit;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.ProgramUnit;
import io.proleap.cobol.asg.metamodel.data.DataDivision;
import io.proleap.cobol.asg.metamodel.data.datadescription.DataDescriptionEntry;
import io.proleap.cobol.asg.metamodel.data.file.FileDescriptionEntry;
import io.proleap.cobol.asg.metamodel.procedure.ProcedureDivision;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor.CobolSourceFormatEnum;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.util.List;

public class DetailedCobolParser {

    public static void main(String[] args) {
        PrintWriter writer = null;
        try {
            System.out.println("=== Detailed COBOL Structure Analyzer ===");
            System.out.println();

            // Create output file
            String outputPath = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/detailed-structure.txt";
            writer = new PrintWriter(new FileWriter(outputPath));

            writer.println("=== DETAILED COBOL PROGRAM STRUCTURE ===");
            writer.println("Generated: " + new java.util.Date());
            writer.println();

            // COBOL file to parse
            File cobolFile = new File("/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/docs/parser/copybooks/RG1866B_unix.cbl");

            System.out.println("Parsing: " + cobolFile.getName());
            writer.println("Source File: " + cobolFile.getName());
            writer.println("File Size: " + cobolFile.length() + " bytes");
            writer.println();

            // Initialize and run parser
            CobolParserRunnerImpl runner = new CobolParserRunnerImpl();
            Program program = runner.analyzeFile(cobolFile, CobolSourceFormatEnum.FIXED);

            if (program == null || program.getCompilationUnits().size() == 0) {
                System.err.println("Failed to parse file");
                writer.println("ERROR: Failed to parse file");
                writer.close();
                System.exit(1);
            }

            System.out.println("Parsing successful!");
            System.out.println();

            // Process compilation unit
            CompilationUnit unit = program.getCompilationUnits().get(0);
            ProgramUnit programUnit = unit.getProgramUnit();

            writer.println("Program Name: " + unit.getName());
            writer.println();
            writer.println("========================================");
            writer.println("DETAILED STRUCTURE ANALYSIS");
            writer.println("========================================");
            writer.println();

            // Analyze Data Division
            if (programUnit.getDataDivision() != null) {
                analyzeDataDivision(programUnit.getDataDivision(), writer);
            }

            // Analyze Procedure Division
            if (programUnit.getProcedureDivision() != null) {
                analyzeProcedureDivision(programUnit.getProcedureDivision(), writer);
            }

            writer.println();
            writer.println("=== END OF DETAILED STRUCTURE ===");
            writer.close();

            System.out.println("Analysis complete!");
            System.out.println("Output saved to: " + outputPath);

        } catch (Exception e) {
            System.err.println("ERROR: " + e.getMessage());
            e.printStackTrace();
            if (writer != null) {
                writer.println("ERROR: " + e.getMessage());
                e.printStackTrace(writer);
                writer.close();
            }
            System.exit(1);
        }
    }

    private static void analyzeDataDivision(DataDivision dataDiv, PrintWriter writer) {
        writer.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        writer.println("DATA DIVISION - DETAILED ANALYSIS");
        writer.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        writer.println();

        // File Section
        if (dataDiv.getFileSection() != null) {
            writer.println("┌─ FILE SECTION");
            writer.println("│");
            List<FileDescriptionEntry> files = dataDiv.getFileSection().getFileDescriptionEntries();
            writer.println("│  Total Files: " + files.size());
            writer.println("│");

            for (int i = 0; i < files.size(); i++) {
                FileDescriptionEntry file = files.get(i);
                String prefix = (i == files.size() - 1) ? "└──" : "├──";
                writer.println("│  " + prefix + " FD: " + file.getName());
            }
            writer.println();
        }

        // Working Storage Section
        if (dataDiv.getWorkingStorageSection() != null) {
            writer.println("┌─ WORKING-STORAGE SECTION");
            writer.println("│");
            List<DataDescriptionEntry> dataItems = dataDiv.getWorkingStorageSection().getDataDescriptionEntries();
            writer.println("│  Total Data Items: " + dataItems.size());
            writer.println("│");

            // Count by level
            int level01 = 0, level05 = 0, level10 = 0, level77 = 0, level88 = 0, others = 0;

            for (DataDescriptionEntry item : dataItems) {
                Integer level = item.getLevelNumber();
                if (level != null) {
                    if (level == 1) level01++;
                    else if (level == 5) level05++;
                    else if (level == 10) level10++;
                    else if (level == 77) level77++;
                    else if (level == 88) level88++;
                    else others++;
                }
            }

            writer.println("│  Data Item Distribution:");
            writer.println("│    Level 01 (Group/Record): " + level01);
            writer.println("│    Level 05 (Group/Field): " + level05);
            writer.println("│    Level 10 (Field): " + level10);
            writer.println("│    Level 77 (Independent): " + level77);
            writer.println("│    Level 88 (Condition): " + level88);
            writer.println("│    Other Levels: " + others);
            writer.println("│");

            // List first 50 Level 01 items
            writer.println("│  Level 01 Data Structures (first 50):");
            int count = 0;
            for (DataDescriptionEntry item : dataItems) {
                if (item.getLevelNumber() != null && item.getLevelNumber() == 1) {
                    if (count < 50) {
                        String name = item.getName() != null ? item.getName() : "(unnamed)";
                        writer.println("│    ├── " + name);
                        count++;
                    }
                }
            }
            if (level01 > 50) {
                writer.println("│    └── ... and " + (level01 - 50) + " more");
            }
            writer.println();
        }

        // Linkage Section
        if (dataDiv.getLinkageSection() != null) {
            writer.println("┌─ LINKAGE SECTION");
            writer.println("│");
            List<DataDescriptionEntry> linkageItems = dataDiv.getLinkageSection().getDataDescriptionEntries();
            writer.println("│  Total Linkage Items: " + linkageItems.size());
            writer.println();
        }
    }

    private static void analyzeProcedureDivision(ProcedureDivision procDiv, PrintWriter writer) {
        writer.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        writer.println("PROCEDURE DIVISION - DETAILED ANALYSIS");
        writer.println("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        writer.println();

        // Sections
        if (procDiv.getSections() != null && !procDiv.getSections().isEmpty()) {
            writer.println("┌─ SECTIONS");
            writer.println("│");
            writer.println("│  Total Sections: " + procDiv.getSections().size());
            writer.println("│");
            writer.println("│  Section Names:");

            int sectionCount = 0;
            var sections = procDiv.getSections();
            for (var section : sections) {
                String prefix = (sectionCount == sections.size() - 1) ? "└──" : "├──";
                String name = section.getName() != null ? section.getName() : "(unnamed)";
                writer.println("│  " + prefix + " " + name);
                sectionCount++;
            }
            writer.println();
        }

        // Paragraphs
        if (procDiv.getParagraphs() != null && !procDiv.getParagraphs().isEmpty()) {
            writer.println("┌─ PARAGRAPHS");
            writer.println("│");
            writer.println("│  Total Paragraphs: " + procDiv.getParagraphs().size());
            writer.println("│");
            writer.println("│  Paragraph Names:");

            int paragraphCount = 0;
            var paragraphs = procDiv.getParagraphs();
            for (var paragraph : paragraphs) {
                String prefix = (paragraphCount == paragraphs.size() - 1) ? "└──" : "├──";
                String name = paragraph.getName() != null ? paragraph.getName() : "(unnamed)";
                writer.println("│  " + prefix + " " + name);
                paragraphCount++;
            }
            writer.println();
        }

        // Statements
        if (procDiv.getStatements() != null) {
            writer.println("┌─ STATEMENTS");
            writer.println("│");
            writer.println("│  Total Statements: " + procDiv.getStatements().size());
            writer.println("│");
            writer.println("│  (Detailed statement analysis available)");
            writer.println();
        }
    }
}
