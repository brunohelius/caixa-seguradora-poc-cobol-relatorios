using System.Text;

namespace CaixaSeguradora.ComparisonTests
{
    /// <summary>
    /// Tests for the OutputValidator class to ensure it correctly validates byte-level compatibility
    /// </summary>
    public class OutputValidatorTests
    {
        private readonly OutputValidator _validator;
        private readonly string _testDirectory;

        public OutputValidatorTests()
        {
            _validator = new OutputValidator();
            _testDirectory = Path.Combine(Path.GetTempPath(), "CaixaSeguradora.ComparisonTests");
            Directory.CreateDirectory(_testDirectory);
        }

        [Fact]
        public void CompareFiles_IdenticalFiles_ReturnsMatch()
        {
            // Arrange
            var filePath1 = Path.Combine(_testDirectory, "identical1.txt");
            var filePath2 = Path.Combine(_testDirectory, "identical2.txt");

            var content = "PREMIT000000000001000000000002000000000003";
            File.WriteAllText(filePath1, content);
            File.WriteAllText(filePath2, content);

            // Act
            OutputValidator.ComparisonResult result = _validator.CompareFiles(filePath1, filePath2);

            // Assert
            Assert.True(result.Match);
            Assert.Null(result.Error);
        }

        [Fact]
        public void CompareFiles_DifferentFiles_ReturnsMismatch()
        {
            // Arrange
            var filePath1 = Path.Combine(_testDirectory, "different1.txt");
            var filePath2 = Path.Combine(_testDirectory, "different2.txt");

            File.WriteAllText(filePath1, "PREMIT000000000001000000000002");
            File.WriteAllText(filePath2, "PREMIT000000000001000000000003");

            // Act
            OutputValidator.ComparisonResult result = _validator.CompareFiles(filePath1, filePath2);

            // Assert
            Assert.False(result.Match);
            Assert.NotNull(result.Error);
            Assert.Contains("Byte mismatch", result.Error);
        }

        [Fact]
        public void CompareFiles_DifferentSizes_ReturnsMismatch()
        {
            // Arrange
            var filePath1 = Path.Combine(_testDirectory, "size1.txt");
            var filePath2 = Path.Combine(_testDirectory, "size2.txt");

            File.WriteAllText(filePath1, "PREMIT000000000001");
            File.WriteAllText(filePath2, "PREMIT000000000001000000000002");

            // Act
            OutputValidator.ComparisonResult result = _validator.CompareFiles(filePath1, filePath2);

            // Assert
            Assert.False(result.Match);
            Assert.NotNull(result.Error);
            Assert.Contains("File size mismatch", result.Error);
        }

        [Fact]
        public void CompareFiles_MissingFiles_ReturnsError()
        {
            // Arrange
            var missingPath = Path.Combine(_testDirectory, "missing.txt");
            var existingPath = Path.Combine(_testDirectory, "existing.txt");
            File.WriteAllText(existingPath, "PREMIT000000000001");

            // Act
            OutputValidator.ComparisonResult result = _validator.CompareFiles(missingPath, existingPath);

            // Assert
            Assert.False(result.Match);
            Assert.NotNull(result.Error);
            Assert.Contains("does not exist", result.Error);
        }

        [Fact]
        public void CompareFiles_ByteMismatch_ContextProvided()
        {
            // Arrange
            var filePath1 = Path.Combine(_testDirectory, "mismatch1.txt");
            var filePath2 = Path.Combine(_testDirectory, "mismatch2.txt");

            File.WriteAllText(filePath1, "PREMIT000000000001000000000002");
            File.WriteAllText(filePath2, "PREMIT000000000002000000000002"); // Different at position 15

            // Act
            OutputValidator.ComparisonResult result = _validator.CompareFiles(filePath1, filePath2);

            // Assert
            Assert.False(result.Match);
            Assert.NotNull(result.Context);
            Assert.Contains("PREMIT", result.Context);
        }
    }
}
