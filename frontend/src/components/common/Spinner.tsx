import React from 'react';

export interface SpinnerProps {
  size?: 'small' | 'medium' | 'large';
  color?: 'blue' | 'white' | 'gray';
  className?: string;
}

const Spinner: React.FC<SpinnerProps> = ({
  size = 'medium',
  color = 'blue',
  className = '',
}) => {
  const getSizeStyle = () => {
    const sizes = {
      small: { width: '20px', height: '20px' },
      medium: { width: '40px', height: '40px' },
      large: { width: '60px', height: '60px' },
    };
    return sizes[size];
  };

  const getColorStyle = () => {
    const colors = {
      blue: '#7ac0da',
      white: '#fff',
      gray: '#666',
    };
    return colors[color];
  };

  return (
    <svg
      style={{
        ...getSizeStyle(),
        animation: 'spin 1s linear infinite',
        color: getColorStyle(),
      }}
      className={className}
      xmlns="http://www.w3.org/2000/svg"
      fill="none"
      viewBox="0 0 24 24"
    >
      <style>
        {`
          @keyframes spin {
            from { transform: rotate(0deg); }
            to { transform: rotate(360deg); }
          }
        `}
      </style>
      <circle
        style={{ opacity: 0.25 }}
        cx="12"
        cy="12"
        r="10"
        stroke="currentColor"
        strokeWidth="4"
      ></circle>
      <path
        style={{ opacity: 0.75 }}
        fill="currentColor"
        d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
      ></path>
    </svg>
  );
};

export default Spinner;
