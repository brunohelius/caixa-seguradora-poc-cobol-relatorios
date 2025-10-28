using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Markdig;
using Markdig.Syntax;
using Markdig.Syntax.Inlines;
using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration
{
    /// <summary>
    /// Renders markdown content to PDF format using Markdig parser.
    /// </summary>
    public class MarkdownToPdfRenderer
    {
        private readonly BrandingStyles _brandingStyles;
        private readonly TableFormatter _tableFormatter;
        private readonly MarkdownPipeline _pipeline;

        public MarkdownToPdfRenderer()
        {
            _brandingStyles = new BrandingStyles();
            _tableFormatter = new TableFormatter();

            // Configure Markdig pipeline with extensions
            _pipeline = new MarkdownPipelineBuilder()
                .UseAdvancedExtensions()
                .UseYamlFrontMatter()
                .UseEmphasisExtras()
                .UsePipeTables()
                .UseListExtras()
                .UseTaskLists()
                .UseAutoLinks()
                .Build();
        }

        /// <summary>
        /// Renders markdown content to PDF container.
        /// </summary>
        public void RenderMarkdown(IContainer container, string markdown)
        {
            if (string.IsNullOrWhiteSpace(markdown))
                return;

            var document = Markdown.Parse(markdown, _pipeline);

            container.Column(column =>
            {
                foreach (var block in document)
                {
                    RenderBlock(column, block);
                }
            });
        }

        /// <summary>
        /// Renders a markdown block element.
        /// </summary>
        private void RenderBlock(ColumnDescriptor column, Block block)
        {
            switch (block)
            {
                case HeadingBlock heading:
                    RenderHeading(column, heading);
                    break;

                case ParagraphBlock paragraph:
                    RenderParagraph(column, paragraph);
                    break;

                case ListBlock list:
                    RenderList(column, list);
                    break;

                case CodeBlock code:
                    RenderCodeBlock(column, code);
                    break;

                case QuoteBlock quote:
                    RenderQuote(column, quote);
                    break;

                case Markdig.Extensions.Tables.Table table:
                    RenderTable(column, table);
                    break;

                case ThematicBreakBlock:
                    RenderHorizontalRule(column);
                    break;

                default:
                    // Handle other block types or skip
                    break;
            }
        }

        /// <summary>
        /// Renders a heading block.
        /// </summary>
        private void RenderHeading(ColumnDescriptor column, HeadingBlock heading)
        {
            var level = heading.Level;
            var text = ExtractText(heading.Inline);

            column.Item()
                .PaddingTop(level == 1 ? 20 : 10)
                .PaddingBottom(5)
                .Text(text)
                .FontSize(GetHeadingFontSize(level))
                .Bold()
                .FontColor(_brandingStyles.PrimaryColor);
        }

        /// <summary>
        /// Renders a paragraph block.
        /// </summary>
        private void RenderParagraph(ColumnDescriptor column, ParagraphBlock paragraph)
        {
            column.Item()
                .PaddingVertical(5)
                .Text(text =>
                {
                    RenderInlines(text, paragraph.Inline);
                });
        }

        /// <summary>
        /// Renders inline elements within text.
        /// </summary>
        private void RenderInlines(TextDescriptor text, ContainerInline inlines)
        {
            foreach (var inline in inlines)
            {
                switch (inline)
                {
                    case LiteralInline literal:
                        text.Span(literal.Content.ToString())
                            .FontSize(_brandingStyles.BodyFontSize)
                            .FontColor(_brandingStyles.TextColor);
                        break;

                    case EmphasisInline emphasis:
                        var emphasisText = ExtractText(emphasis);
                        if (emphasis.DelimiterCount == 2) // Bold
                        {
                            text.Span(emphasisText)
                                .FontSize(_brandingStyles.BodyFontSize)
                                .Bold()
                                .FontColor(_brandingStyles.TextColor);
                        }
                        else // Italic
                        {
                            text.Span(emphasisText)
                                .FontSize(_brandingStyles.BodyFontSize)
                                .Italic()
                                .FontColor(_brandingStyles.TextColor);
                        }
                        break;

                    case CodeInline code:
                        text.Span(" ")
                            .FontSize(_brandingStyles.BodyFontSize);
                        text.Span(code.Content)
                            .FontFamily(_brandingStyles.MonospaceFont)
                            .FontSize(_brandingStyles.SmallFontSize)
                            .BackgroundColor("#F5F5F5")
                            .FontColor(_brandingStyles.DarkBlue);
                        text.Span(" ")
                            .FontSize(_brandingStyles.BodyFontSize);
                        break;

                    case LinkInline link:
                        var linkText = ExtractText(link);
                        text.Span(linkText)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .FontColor(_brandingStyles.PrimaryColor)
                            .Underline();
                        break;

                    case LineBreakInline:
                        text.Line(" ");
                        break;

                    default:
                        // Handle other inline types or extract text
                        var defaultText = ExtractText(inline);
                        if (!string.IsNullOrEmpty(defaultText))
                        {
                            text.Span(defaultText)
                                .FontSize(_brandingStyles.BodyFontSize)
                                .FontColor(_brandingStyles.TextColor);
                        }
                        break;
                }
            }
        }

        /// <summary>
        /// Renders a list block.
        /// </summary>
        private void RenderList(ColumnDescriptor column, ListBlock list)
        {
            column.Item()
                .PaddingVertical(5)
                .Column(listColumn =>
                {
                    var index = 1;
                    foreach (var item in list)
                    {
                        if (item is ListItemBlock listItem)
                        {
                            RenderListItem(listColumn, listItem, list.IsOrdered, index);
                            index++;
                        }
                    }
                });
        }

        /// <summary>
        /// Renders a list item.
        /// </summary>
        private void RenderListItem(ColumnDescriptor column, ListItemBlock item, bool isOrdered, int index)
        {
            column.Item()
                .PaddingLeft(20)
                .Row(row =>
                {
                    // Bullet or number
                    row.ConstantItem(30)
                        .Text(isOrdered ? $"{index}." : "•")
                        .FontSize(_brandingStyles.BodyFontSize)
                        .FontColor(_brandingStyles.TextColor);

                    // Item content
                    row.RelativeItem()
                        .Column(itemColumn =>
                        {
                            foreach (var block in item)
                            {
                                if (block is ParagraphBlock paragraph)
                                {
                                    itemColumn.Item()
                                        .Text(text =>
                                        {
                                            RenderInlines(text, paragraph.Inline);
                                        });
                                }
                                else if (block is ListBlock nestedList)
                                {
                                    RenderList(itemColumn, nestedList);
                                }
                            }
                        });
                });
        }

        /// <summary>
        /// Renders a code block.
        /// </summary>
        private void RenderCodeBlock(ColumnDescriptor column, CodeBlock code)
        {
            var codeText = GetCodeBlockText(code);
            var language = GetCodeLanguage(code);

            column.Item()
                .PaddingVertical(5)
                .Background("#F5F5F5")
                .Border(1)
                .BorderColor(_brandingStyles.TableBorderColor)
                .Padding(10)
                .Column(codeColumn =>
                {
                    // Language label if available
                    if (!string.IsNullOrEmpty(language))
                    {
                        codeColumn.Item()
                            .Text(language)
                            .FontSize(_brandingStyles.FootnoteFontSize)
                            .FontColor(_brandingStyles.SecondaryTextColor)
                            .Bold();
                    }

                    // Code content
                    codeColumn.Item()
                        .Text(codeText)
                        .FontFamily(_brandingStyles.MonospaceFont)
                        .FontSize(_brandingStyles.SmallFontSize)
                        .FontColor(_brandingStyles.DarkBlue);
                });
        }

        /// <summary>
        /// Renders a quote block.
        /// </summary>
        private void RenderQuote(ColumnDescriptor column, QuoteBlock quote)
        {
            column.Item()
                .PaddingVertical(5)
                .BorderLeft(3)
                .BorderColor(_brandingStyles.SecondaryColor)
                .PaddingLeft(15)
                .Column(quoteColumn =>
                {
                    foreach (var block in quote)
                    {
                        RenderBlock(quoteColumn, block);
                    }
                });
        }

        /// <summary>
        /// Renders a markdown table.
        /// </summary>
        private void RenderTable(ColumnDescriptor column, Markdig.Extensions.Tables.Table table)
        {
            var tableData = new TableData();

            // Extract columns from header
            if (table.Count > 0 && table[0] is Markdig.Extensions.Tables.TableRow headerRow)
            {
                foreach (Markdig.Extensions.Tables.TableCell cell in headerRow)
                {
                    tableData.Columns.Add(new TableColumn
                    {
                        Title = ExtractText(cell),
                        Alignment = GetTableAlignment(cell)
                    });
                }
            }

            // Extract data rows
            for (int i = 1; i < table.Count; i++)
            {
                if (table[i] is Markdig.Extensions.Tables.TableRow dataRow)
                {
                    var row = new TableRow();
                    foreach (Markdig.Extensions.Tables.TableCell cell in dataRow)
                    {
                        row.Cells.Add(new TableCell
                        {
                            Value = ExtractText(cell)
                        });
                    }
                    tableData.Rows.Add(row);
                }
            }

            column.Item()
                .PaddingVertical(10)
                .Element(container => _tableFormatter.CreateTable(container, tableData));
        }

        /// <summary>
        /// Renders a horizontal rule.
        /// </summary>
        private void RenderHorizontalRule(ColumnDescriptor column)
        {
            column.Item()
                .PaddingVertical(10)
                .Height(1)
                .Background(_brandingStyles.TableBorderColor);
        }

        /// <summary>
        /// Extracts text content from inline elements.
        /// </summary>
        private string ExtractText(Inline inline)
        {
            if (inline == null) return string.Empty;

            var text = "";
            foreach (var child in inline.Descendants())
            {
                if (child is LiteralInline literal)
                {
                    text += literal.Content.ToString();
                }
            }
            return text;
        }

        /// <summary>
        /// Extracts text content from a block.
        /// </summary>
        private string ExtractText(Block block)
        {
            if (block is LeafBlock leafBlock && leafBlock.Inline != null)
            {
                return ExtractText(leafBlock.Inline);
            }
            return string.Empty;
        }

        /// <summary>
        /// Gets the font size for a heading level.
        /// </summary>
        private float GetHeadingFontSize(int level)
        {
            return level switch
            {
                1 => _brandingStyles.TitleFontSize,
                2 => _brandingStyles.HeadingFontSize,
                3 => _brandingStyles.SubheadingFontSize,
                4 => _brandingStyles.BodyFontSize + 2,
                5 => _brandingStyles.BodyFontSize + 1,
                _ => _brandingStyles.BodyFontSize
            };
        }

        /// <summary>
        /// Gets the text content from a code block.
        /// </summary>
        private string GetCodeBlockText(CodeBlock code)
        {
            if (code is Markdig.Syntax.FencedCodeBlock fenced)
            {
                return fenced.Lines.ToString();
            }
            return code.Lines?.ToString() ?? string.Empty;
        }

        /// <summary>
        /// Gets the language identifier from a fenced code block.
        /// </summary>
        private string GetCodeLanguage(CodeBlock code)
        {
            if (code is Markdig.Syntax.FencedCodeBlock fenced)
            {
                return fenced.Info ?? string.Empty;
            }
            return string.Empty;
        }

        /// <summary>
        /// Gets table cell alignment.
        /// </summary>
        private TextAlignment GetTableAlignment(Markdig.Extensions.Tables.TableCell cell)
        {
            if (cell.ColumnAlignment.HasValue)
            {
                return cell.ColumnAlignment.Value switch
                {
                    Markdig.Extensions.Tables.TableColumnAlign.Center => TextAlignment.Center,
                    Markdig.Extensions.Tables.TableColumnAlign.Right => TextAlignment.Right,
                    _ => TextAlignment.Left
                };
            }
            return TextAlignment.Left;
        }
    }
}