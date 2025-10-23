using System.ComponentModel.DataAnnotations;

namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO for user authentication.
/// </summary>
public class LoginRequest
{
    /// <summary>
    /// Username for authentication.
    /// </summary>
    [Required(ErrorMessage = "Nome de usuário é obrigatório")]
    [StringLength(100, MinimumLength = 3, ErrorMessage = "Nome de usuário deve ter entre 3 e 100 caracteres")]
    public string Username { get; set; } = string.Empty;

    /// <summary>
    /// User password.
    /// </summary>
    [Required(ErrorMessage = "Senha é obrigatória")]
    [StringLength(100, MinimumLength = 6, ErrorMessage = "Senha deve ter no mínimo 6 caracteres")]
    public string Password { get; set; } = string.Empty;
}
