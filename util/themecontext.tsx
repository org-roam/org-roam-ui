import { createContext } from 'react'

import { themes } from '../components/themes'
const initialTheme = ['vibrant', themes['one-vibrant']]
type Theme = [name: string, themeObject: { [color: string]: string }]

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
