/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "templates/**/*.html"
  ],
  theme: {
    fontFamily: {
      sans: ['IBM Plex Sans', 'OPPO Sans', 'sans-serif'],
      serif: ['IBM Plex Serif', 'Source Han Serif CN', 'serif'],
      mono: ['IBM Plex Mono', 'monospace'],
    },
    fontSize: {
      'xs': ['0.75rem', {lineHeight: '1.1rem'}],
      'sm': ['0.9rem', {lineHeight: '1.4rem'}],
      'base': ['1.05rem', {lineHeight: '1.62rem'}],
      'lg': ['1.15rem', {lineHeight: '1.78rem'}],
      'xl': ['1.275rem', {lineHeight: '1.9rem'}],
      '2xl': ['1.4rem', {lineHeight: '2rem'}],
      '3xl': ['1.65rem', {lineHeight: '2.2rem'}],
      '4xl': ['1.85rem', {lineHeight: '2.35rem'}],
      '5xl': ['2.2rem', {lineHeight: '2.5rem'}],
    },
    extend: {},
  },
  plugins: [],
}
