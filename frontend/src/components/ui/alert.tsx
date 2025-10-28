import * as React from "react"
import { cva, type VariantProps } from "class-variance-authority"

import { cn } from "@/lib/utils"

const alertVariants = cva(
  "relative w-full rounded-lg border-2 p-4 [&>svg~*]:pl-7 [&>svg+div]:translate-y-[-3px] [&>svg]:absolute [&>svg]:left-4 [&>svg]:top-4",
  {
    variants: {
      variant: {
        default: "bg-white border-site-grayDark text-site-text",
        destructive: "bg-red-50 border-site-error text-site-error [&>svg]:text-site-error",
        warning: "bg-yellow-50 border-warning text-yellow-900 [&>svg]:text-warning",
        info: "bg-blue-50 border-info text-blue-900 [&>svg]:text-info",
        success: "bg-green-50 border-success text-green-900 [&>svg]:text-success",
        successBanner: "bg-gradient-to-r from-green-500 to-emerald-600 border-green-600 text-white [&>svg]:text-white shadow-2xl",
        infoBanner: "bg-gradient-to-r from-caixa-blue to-indigo-600 border-caixa-blue text-white [&>svg]:text-white shadow-2xl",
      },
    },
    defaultVariants: {
      variant: "destructive",
    },
  }
)

const Alert = React.forwardRef<
  HTMLDivElement,
  React.HTMLAttributes<HTMLDivElement> & VariantProps<typeof alertVariants>
>(({ className, variant, ...props }, ref) => (
  <div
    ref={ref}
    role="alert"
    className={cn(alertVariants({ variant }), className)}
    {...props}
  />
))
Alert.displayName = "Alert"

const AlertTitle = React.forwardRef<
  HTMLParagraphElement,
  React.HTMLAttributes<HTMLHeadingElement>
>(({ className, ...props }, ref) => (
  <h5
    ref={ref}
    className={cn("mb-1 font-medium leading-none tracking-tight", className)}
    {...props}
  />
))
AlertTitle.displayName = "AlertTitle"

const AlertDescription = React.forwardRef<
  HTMLParagraphElement,
  React.HTMLAttributes<HTMLParagraphElement>
>(({ className, ...props }, ref) => (
  <div
    ref={ref}
    className={cn("text-sm [&_p]:leading-relaxed", className)}
    {...props}
  />
))
AlertDescription.displayName = "AlertDescription"

export { Alert, AlertTitle, AlertDescription }
