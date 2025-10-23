using CaixaSeguradora.Core.DTOs;
using FluentValidation;

namespace CaixaSeguradora.Api.Validators;

/// <summary>
/// Validator for LoginRequest DTO.
/// Validates authentication credentials according to system requirements.
/// </summary>
public class LoginRequestValidator : AbstractValidator<LoginRequest>
{
    public LoginRequestValidator()
    {
        RuleFor(x => x.Username)
            .NotEmpty()
            .WithMessage("Nome de usuário é obrigatório")
            .MaximumLength(50)
            .WithMessage("Nome de usuário não pode exceder 50 caracteres")
            .MinimumLength(3)
            .WithMessage("Nome de usuário deve ter no mínimo 3 caracteres");

        RuleFor(x => x.Password)
            .NotEmpty()
            .WithMessage("Senha é obrigatória")
            .MinimumLength(6)
            .WithMessage("Senha deve ter no mínimo 6 caracteres")
            .MaximumLength(100)
            .WithMessage("Senha não pode exceder 100 caracteres");
    }
}
