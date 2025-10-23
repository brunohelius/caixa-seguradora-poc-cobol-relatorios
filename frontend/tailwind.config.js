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
        // Caixa Seguradora Brand Colors (from Site.css)
        caixa: {
          blue: {
            DEFAULT: '#0047BB', // Azul Caixa principal
            dark: '#003380',    // Azul escuro
            light: '#E6F0FF',   // Azul claro para fundos
          },
          yellow: {
            DEFAULT: '#FFB81C', // Amarelo Caixa
            dark: '#E6A519',    // Amarelo escuro
          },
          gray: {
            900: '#1A1A1A',     // Texto muito escuro
            700: '#4A4A4A',     // Texto médio
            400: '#BDBDBD',     // Cinza médio
            100: '#F5F5F5',     // Fundo claro
          },
        },
        // Site.css theme colors
        site: {
          blue: '#7ac0da',      // Azul claro Site.css
          blueLight: '#a4d4e6', // Azul gradiente
          gray: '#efeeef',      // Cinza fundo
          grayDark: '#e2e2e2',  // Cinza borda
          text: '#333',         // Texto padrão
          error: '#e80c4d',     // Vermelho erro
        },
        // Status colors
        success: '#28A745',
        error: '#DC3545',
        warning: '#FFC107',
        info: '#17A2B8',

        // shadcn/ui semantic colors mapped to Caixa brand
        border: "#e2e2e2",
        input: "#e2e2e2",
        ring: "#0047BB",
        background: "#ffffff",
        foreground: "#333333",
        primary: {
          DEFAULT: "#0047BB",
          foreground: "#ffffff",
        },
        secondary: {
          DEFAULT: "#FFB81C",
          foreground: "#000000",
        },
        destructive: {
          DEFAULT: "#DC3545",
          foreground: "#ffffff",
        },
        muted: {
          DEFAULT: "#efeeef",
          foreground: "#666666",
        },
        accent: {
          DEFAULT: "#7ac0da",
          foreground: "#000000",
        },
        popover: {
          DEFAULT: "#ffffff",
          foreground: "#333333",
        },
        card: {
          DEFAULT: "#ffffff",
          foreground: "#333333",
        },
      },
      borderRadius: {
        lg: "0.75rem",
        md: "0.5rem",
        sm: "0.25rem",
      },
      fontFamily: {
        sans: ['Segoe UI', 'Verdana', 'Helvetica', 'sans-serif'],
      },
    },
  },
  plugins: [],
}

