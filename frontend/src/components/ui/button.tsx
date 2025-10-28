import * as React from "react"
import { Slot } from "@radix-ui/react-slot"
import { cva, type VariantProps } from "class-variance-authority"

import { cn } from "@/lib/utils"

const buttonVariants = cva(
  "inline-flex items-center justify-center gap-2 whitespace-nowrap rounded font-semibold transition-all duration-200 focus-visible:outline-none focus-visible:ring-2 focus-visible:ring-caixa-blue focus-visible:ring-offset-2 disabled:pointer-events-none disabled:opacity-60 [&_svg]:pointer-events-none [&_svg]:size-4 [&_svg]:shrink-0",
  {
    variants: {
      variant: {
        // Caixa brand primary blue
        primary: "bg-site-blue text-white border border-[#5a9fb8] hover:bg-[#6bb0d0] active:bg-[#5a9fb8]",
        // Caixa brand secondary yellow
        secondary: "bg-caixa-yellow text-black border border-caixa-yellow-dark hover:bg-caixa-yellow-dark",
        // Danger/error state
        danger: "bg-site-error text-white border border-[#c00a3e] hover:bg-[#d00b44] active:bg-[#c00a3e]",
        // Success state
        success: "bg-success text-white border border-[#218838] hover:bg-[#239a3e] active:bg-[#218838]",
        // Default gray variant
        default: "bg-[#d3dce0] text-site-text border border-[#787878] hover:bg-[#c4cdd1] active:bg-[#b5bec2]",
        // Outline variant
        outline: "border-2 border-caixa-blue bg-white text-caixa-blue hover:bg-caixa-blue-light",
        // Ghost variant
        ghost: "hover:bg-site-gray hover:text-site-text",
        // Link variant
        link: "text-caixa-blue underline-offset-4 hover:underline",
      },
      size: {
        small: "text-sm px-2 py-1 min-h-[28px]",
        medium: "text-base px-[10px] py-[7px] min-h-[36px]",
        large: "text-lg px-[14px] py-[10px] min-h-[44px]",
        default: "text-base px-[10px] py-[7px] min-h-[36px]",
        icon: "h-10 w-10",
      },
      fullWidth: {
        true: "w-full",
        false: "",
      }
    },
    defaultVariants: {
      variant: "primary",
      size: "medium",
      fullWidth: false,
    },
  }
)

export interface ButtonProps
  extends React.ButtonHTMLAttributes<HTMLButtonElement>,
    VariantProps<typeof buttonVariants> {
  asChild?: boolean
  loading?: boolean
  fullWidth?: boolean
}

const Button = React.forwardRef<HTMLButtonElement, ButtonProps>(
  ({ className, variant, size, fullWidth, loading = false, disabled, children, asChild = false, ...props }, ref) => {
    const Comp = asChild ? Slot : "button"
    return (
      <Comp
        className={cn(buttonVariants({ variant, size, fullWidth, className }), "mr-2")}
        ref={ref}
        disabled={disabled || loading}
        {...props}
      >
        {loading ? 'Carregando...' : children}
      </Comp>
    )
  }
)
Button.displayName = "Button"

export { Button, buttonVariants }
