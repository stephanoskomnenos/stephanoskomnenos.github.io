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
      'sm': ['0.875rem', {lineHeight: '1.35rem'}],
      'base': ['1rem', {lineHeight: '1.6rem'}],
      'lg': ['1.125rem', {lineHeight: '1.75rem'}],
      'xl': ['1.25rem', {lineHeight: '1.85rem'}],
      '2xl': ['1.4rem', {lineHeight: '2rem'}],
      '3xl': ['1.75rem', {lineHeight: '2.2rem'}],
      '4xl': ['1.9rem', {lineHeight: '2.35rem'}],
      '5xl': ['2.25rem', {lineHeight: '2.5rem'}],
    },
    extend: {},
  },
  plugins: [],
}
