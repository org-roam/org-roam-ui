import { createContext } from 'react'

const initialTheme = {
  base1: '#1c1f24',
  base2: '#21272d',
  base3: '#23272e',
  base4: '#484854',
  base5: '#62686E',
  base6: '#757B80',
  base7: '#9ca0a4',
  base8: '#DFDFDF',
  bg: '#242730',
  'bg-alt': '#2a2e38',
  blue: '#51afef',
  cyan: '#5cEfFF',
  'dark-blue': '#1f5582',
  'dark-cyan': '#6A8FBF',
  fg: '#bbc2cf',
  'fg-alt': '#5D656B',
  green: '#7bc275',
  grey: '#484854',
  magenta: '#C57BDB',
  orange: '#e69055',
  red: '#ff665c',
  teal: '#4db5bd',
  violet: '#a991f1',
  yellow: '#FCCE7B',
}

export interface ThemeContextProps {
  emacsTheme: typeof initialTheme
  setEmacsTheme: any
  highlightColor: string
  setHighlightColor: any
}

const ThemeContext = createContext<ThemeContextProps>({
  emacsTheme: initialTheme,
  setEmacsTheme: null,
  highlightColor: 'purple',
  setHighlightColor: null,
})
export { ThemeContext }
