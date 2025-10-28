import * as React from "react"
import { cn } from "@/lib/utils"

interface ProgressProps extends React.HTMLAttributes<HTMLDivElement> {
  value?: number
  indicatorClassName?: string
  variant?: 'default' | 'success' | 'warning' | 'danger'
  gradient?: boolean
}

const Progress = React.forwardRef<HTMLDivElement, ProgressProps>(
  ({ className, value = 0, indicatorClassName, variant = 'default', gradient = false, ...props }, ref) => {
    const getIndicatorClass = () => {
      if (gradient) {
        switch (variant) {
          case 'success':
            return 'bg-gradient-to-r from-success to-green-700';
          case 'warning':
            return 'bg-gradient-to-r from-warning to-yellow-600';
          case 'danger':
            return 'bg-gradient-to-r from-error to-red-700';
          default:
            return 'bg-gradient-to-r from-caixa-blue to-caixa-blue-dark';
        }
      }
      switch (variant) {
        case 'success':
          return 'bg-success';
        case 'warning':
          return 'bg-warning';
        case 'danger':
          return 'bg-error';
        default:
          return 'bg-caixa-blue';
      }
    };

    return (
      <div
        ref={ref}
        className={cn(
          "relative h-6 w-full overflow-hidden rounded-full bg-site-grayDark",
          className
        )}
        {...props}
      >
        <div
          className={cn(
            "h-full w-full flex-1 transition-all duration-500",
            getIndicatorClass(),
            indicatorClassName
          )}
          style={{ transform: `translateX(-${100 - (value || 0)}%)` }}
        />
      </div>
    )
  }
)
Progress.displayName = "Progress"

export { Progress }
