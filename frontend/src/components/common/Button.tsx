import React from 'react';

export interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
  variant?: 'primary' | 'secondary' | 'danger' | 'success';
  size?: 'small' | 'medium' | 'large';
  loading?: boolean;
  fullWidth?: boolean;
}

const Button: React.FC<ButtonProps> = ({
  variant = 'primary',
  size = 'medium',
  loading = false,
  fullWidth = false,
  children,
  className = '',
  disabled,
  ...props
}) => {
  const getButtonStyle = (): React.CSSProperties => {
    const baseStyle: React.CSSProperties = {
      backgroundColor: '#d3dce0',
      border: '1px solid #787878',
      cursor: loading || disabled ? 'not-allowed' : 'pointer',
      fontSize: size === 'small' ? '1em' : size === 'large' ? '1.4em' : '1.2em',
      fontWeight: 600,
      padding: size === 'small' ? '4px 8px' : size === 'large' ? '10px 14px' : '7px 10px',
      marginRight: '8px',
      width: fullWidth ? '100%' : 'auto',
      opacity: disabled ? 0.6 : 1,
    };

    if (variant === 'primary') {
      baseStyle.backgroundColor = '#7ac0da';
      baseStyle.color = '#fff';
      baseStyle.border = '1px solid #5a9fb8';
    } else if (variant === 'danger') {
      baseStyle.backgroundColor = '#e80c4d';
      baseStyle.color = '#fff';
      baseStyle.border = '1px solid #c00a3e';
    } else if (variant === 'success') {
      baseStyle.backgroundColor = '#28A745';
      baseStyle.color = '#fff';
      baseStyle.border = '1px solid #218838';
    }

    return baseStyle;
  };

  return (
    <button
      style={getButtonStyle()}
      disabled={disabled || loading}
      className={className}
      {...props}
    >
      {loading ? 'Carregando...' : children}
    </button>
  );
};

export default Button;
