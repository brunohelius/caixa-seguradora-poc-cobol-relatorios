/** @type {import('tailwindcss').Config} */
export default {
  darkMode: ["class"],
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        // Caixa Seguradora Brand Colors - Modern Palette
        caixa: {
          blue: {
            900: '#001F54',
            800: '#003380',
            700: '#0047BB', // Primary Caixa Blue
            600: '#0052CC',
            500: '#006BE6',
            400: '#4A9FFF',
            300: '#85C1FF',
            200: '#B8DAFF',
            100: '#E3F2FF',
            50: '#F0F8FF',
          },
          yellow: {
            900: '#994D00',
            800: '#CC6600',
            700: '#FF8000',
            600: '#FF9500',
            500: '#FFB81C', // Secondary Caixa Yellow
            400: '#FFC647',
            300: '#FFD470',
            200: '#FFE299',
            100: '#FFF0C2',
            50: '#FFF9EB',
          },
        },
        // Extended Gray Palette
        gray: {
          900: '#111827',
          800: '#1F2937',
          700: '#374151',
          600: '#4B5563',
          500: '#6B7280',
          400: '#9CA3AF',
          300: '#D1D5DB',
          200: '#E5E7EB',
          100: '#F3F4F6',
          50: '#F9FAFB',
        },
        // Semantic Colors
        success: '#10B981',
        error: '#EF4444',
        warning: '#F59E0B',
        info: '#3B82F6',
        // shadcn/ui compatibility
        border: "#E5E7EB",
        input: "#E5E7EB",
        ring: "#0047BB",
        background: "#FFFFFF",
        foreground: "#111827",
        primary: {
          DEFAULT: "#0047BB",
          foreground: "#FFFFFF",
        },
        secondary: {
          DEFAULT: "#FFB81C",
          foreground: "#111827",
        },
        destructive: {
          DEFAULT: "#EF4444",
          foreground: "#FFFFFF",
        },
        muted: {
          DEFAULT: "#F3F4F6",
          foreground: "#6B7280",
        },
        accent: {
          DEFAULT: "#006BE6",
          foreground: "#FFFFFF",
        },
        popover: {
          DEFAULT: "#FFFFFF",
          foreground: "#111827",
        },
        card: {
          DEFAULT: "#FFFFFF",
          foreground: "#111827",
        },
      },
      borderRadius: {
        lg: "0.75rem",
        md: "0.5rem",
        sm: "0.25rem",
      },
      fontFamily: {
        sans: ['Inter', 'Segoe UI', 'Roboto', 'Helvetica Neue', 'sans-serif'],
        mono: ['SF Mono', 'Monaco', 'Consolas', 'monospace'],
      },
      boxShadow: {
        'sm': '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
        'DEFAULT': '0 1px 3px 0 rgba(0, 0, 0, 0.1), 0 1px 2px 0 rgba(0, 0, 0, 0.06)',
        'md': '0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06)',
        'lg': '0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05)',
        'xl': '0 20px 25px -5px rgba(0, 0, 0, 0.1), 0 10px 10px -5px rgba(0, 0, 0, 0.04)',
        '2xl': '0 25px 50px -12px rgba(0, 0, 0, 0.25)',
      },
      keyframes: {
        'fade-in': {
          '0%': { opacity: '0', transform: 'translateY(10px)' },
          '100%': { opacity: '1', transform: 'translateY(0)' },
        },
        'slide-in': {
          '0%': { transform: 'translateX(-100%)' },
          '100%': { transform: 'translateX(0)' },
        },
        'spin': {
          '0%': { transform: 'rotate(0deg)' },
          '100%': { transform: 'rotate(360deg)' },
        },
      },
      animation: {
        'fade-in': 'fade-in 0.4s ease',
        'slide-in': 'slide-in 0.3s ease',
        'spin': 'spin 1s linear infinite',
      },
    },
  },
  plugins: [],
}
