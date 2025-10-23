/** @type {import('tailwindcss').Config} */
export default {
  content: [
    "./index.html",
    "./src/**/*.{js,ts,jsx,tsx}",
  ],
  theme: {
    extend: {
      colors: {
        'caixa-blue': '#0047BB',
        'caixa-yellow': '#FFB81C',
        'caixa-blue-dark': '#003580',
        'caixa-blue-light': '#4A90E2',
      },
    },
  },
  plugins: [],
}

