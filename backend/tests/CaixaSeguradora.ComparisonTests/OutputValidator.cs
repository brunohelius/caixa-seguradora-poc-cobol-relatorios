using System.Text;

namespace CaixaSeguradora.ComparisonTests
{
    /// <summary>
    /// Validates byte-level compatibility between COBOL and .NET generated output files
    /// </summary>
    public class OutputValidator
    {
        public class ComparisonResult
        {
            public bool Match { get; set; }
            public string? Error { get; set; }
            public string? Context { get; set; }
        }

        /// <summary>
        /// Compares two files at the byte level to ensure exact match
        /// </summary>
        /// <param name="cobolFile">Path to the original COBOL output file</param>
        /// <param name="dotnetFile">Path to the .NET generated output file</param>
        /// <returns>Comparison result with match status and error details if any</returns>
        public ComparisonResult CompareFiles(string cobolFile, string dotnetFile)
        {
            if (!File.Exists(cobolFile))
            {
                return new ComparisonResult
                {
                    Match = false,
                    Error = $"COBOL file does not exist: {cobolFile}"
                };
            }

            if (!File.Exists(dotnetFile))
            {
                return new ComparisonResult
                {
                    Match = false,
                    Error = $".NET file does not exist: {dotnetFile}"
                };
            }

            byte[] cobolBytes = File.ReadAllBytes(cobolFile);
            byte[] dotnetBytes = File.ReadAllBytes(dotnetFile);

            if (cobolBytes.Length != dotnetBytes.Length)
            {
                return new ComparisonResult
                {
                    Match = false,
                    Error = $"File size mismatch: COBOL file size: {cobolBytes.Length} bytes, .NET file size: {dotnetBytes.Length} bytes"
                };
            }

            for (int i = 0; i < cobolBytes.Length; i++)
            {
                if (cobolBytes[i] != dotnetBytes[i])
                {
                    return new ComparisonResult
                    {
                        Match = false,
                        Error = $"Byte mismatch at position {i}: COBOL value: {cobolBytes[i]}, .NET value: {dotnetBytes[i]}",
                        Context = GetContext(dotnetBytes, i, 50)
                    };
                }
            }

            return new ComparisonResult { Match = true };
        }

        private string GetContext(byte[] bytes, int position, int contextSize)
        {
            int start = Math.Max(0, position - contextSize);
            int end = Math.Min(bytes.Length, position + contextSize);
            
            var contextBytes = new byte[end - start];
            Array.Copy(bytes, start, contextBytes, 0, contextBytes.Length);
            
            // Convert to readable string, replacing non-printable characters with dots
            var contextBuilder = new StringBuilder();
            foreach (byte b in contextBytes)
            {
                if (b >= 32 && b <= 126) // Printable ASCII range
                {
                    contextBuilder.Append((char)b);
                }
                else
                {
                    contextBuilder.Append('.');
                }
            }
            
            return contextBuilder.ToString();
        }
    }
}