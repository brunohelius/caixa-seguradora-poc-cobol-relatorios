import React from 'react';

export interface CardProps {
  title?: string;
  subtitle?: string;
  children: React.ReactNode;
  className?: string;
  padding?: 'none' | 'small' | 'medium' | 'large';
}

const Card: React.FC<CardProps> = ({
  title,
  subtitle,
  children,
  className = '',
  padding = 'medium',
}) => {
  const paddingClasses = {
    none: 'p-0',
    small: 'p-4',
    medium: 'p-6',
    large: 'p-8',
  };

  return (
    <div className={`bg-white rounded-xl shadow-lg border-2 border-gray-200 ${paddingClasses[padding]} ${className}`}>
      {(title || subtitle) && (
        <div className="mb-6">
          {title && (
            <h3 className="text-2xl font-bold text-gray-900 mb-2">
              {title}
            </h3>
          )}
          {subtitle && (
            <p className="text-base text-gray-600">
              {subtitle}
            </p>
          )}
        </div>
      )}
      <div>{children}</div>
    </div>
  );
};

export default Card;
