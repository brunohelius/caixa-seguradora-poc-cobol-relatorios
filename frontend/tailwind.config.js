/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        caixa: {
          blue: {
            DEFAULT: '#0047BB',
            dark: '#003380',
            light: '#E6F0FF',
          },
          yellow: {
            DEFAULT: '#FFB81C',
            dark: '#E6A519',
          },
          gray: {
            900: '#1A1A1A',
            700: '#4A4A4A',
            400: '#BDBDBD',
            100: '#F5F5F5',
          },
        },
        success: '#28A745',
        error: '#DC3545',
        warning: '#FFC107',
        info: '#17A2B8',
      },
      fontFamily: {
        sans: ['Segoe UI', 'Helvetica Neue', 'Arial', 'sans-serif'],
      },
    },
  },
  plugins: [],
}

