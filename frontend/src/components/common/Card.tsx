import React from 'react';
import {
  Card as ShadcnCard,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle,
} from '../ui/card';
import { cn } from '@/lib/utils';

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
    none: '',
    small: 'p-4',
    medium: 'p-6',
    large: 'p-8',
  };

  // If no title/subtitle and custom padding, apply padding directly to the card
  if (!title && !subtitle && padding !== 'medium') {
    return (
      <ShadcnCard className={cn(paddingClasses[padding], className)}>
        {children}
      </ShadcnCard>
    );
  }

  // Use CardHeader/CardContent structure when we have title/subtitle
  return (
    <ShadcnCard className={className}>
      {(title || subtitle) && (
        <CardHeader className={cn(padding === 'none' ? 'p-0' : undefined)}>
          {title && <CardTitle>{title}</CardTitle>}
          {subtitle && <CardDescription>{subtitle}</CardDescription>}
        </CardHeader>
      )}
      <CardContent className={cn(
        paddingClasses[padding],
        !title && !subtitle ? '' : 'pt-0'
      )}>
        {children}
      </CardContent>
    </ShadcnCard>
  );
};

export default Card;
