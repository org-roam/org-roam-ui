import '../styles/globals.css'
import type { AppProps } from 'next/app'
import { ChakraProvider, extendTheme, withDefaultColorScheme } from '@chakra-ui/react'
import { useEffect, useState, useMemo, useContext, useReducer } from 'react'
import * as d3int from 'd3-interpolate'

import { ThemeContext } from './themecontext'
import { usePersistantState } from '../util/persistant-state'

function MyApp({ Component, pageProps }: AppProps) {
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
  const [isInitialized, setIsInitialized] = useState(false)

  const [emacsTheme, setEmacsTheme] = useState(initialTheme)
  const [highlightColor, setHighlightColor] = useState('purple')

  useEffect(() => {
    if (isInitialized) {
      localStorage.setItem('theme', JSON.stringify(emacsTheme))
    }
  }, [emacsTheme])

  useEffect(() => {
    if (isInitialized) {
      localStorage.setItem('highlightColor', JSON.stringify(highlightColor))
    }
  }, [highlightColor])

  useEffect(() => {
    setEmacsTheme(
      JSON.parse(localStorage.getItem('theme') ?? JSON.stringify(initialTheme)) ?? initialTheme,
    )
    setHighlightColor(
      JSON.parse(localStorage.getItem('highlightColor') ?? JSON.stringify(highlightColor)) ??
        highlightColor,
    )
    setIsInitialized(true)
  }, [])

  const themeObject = {
    emacsTheme: emacsTheme,
    setEmacsTheme: setEmacsTheme,
    highlightColor: highlightColor,
    setHighlightColor: setHighlightColor,
  }
  return (
    <ThemeContext.Provider value={themeObject as typeof themeObject}>
      <SubApp>
        <Component {...pageProps} />
      </SubApp>
    </ThemeContext.Provider>
  )
}

function SubApp(props: any) {
  const { children } = props
  const { highlightColor, emacsTheme } = useContext(ThemeContext)
  // yeah it's annoying, should put this someplace more sensible
  const getBorderColor = () => {
    if (highlightColor === 'purple') {
      return emacsTheme.violet + 'aa'
    }
    if (highlightColor === 'pink') {
      return emacsTheme.magenta + 'aa'
    }
    if (highlightColor === 'blue') {
      return emacsTheme.blue + 'aa'
    }
    if (highlightColor === 'cyan') {
      return emacsTheme.cyan + 'aa'
    }
    if (highlightColor === 'green') {
      return emacsTheme.green + 'aa'
    }
    if (highlightColor === 'yellow') {
      return emacsTheme.yellow + 'aa'
    }
    if (highlightColor === 'orange') {
      return emacsTheme.orange + 'aa'
    }
    if (highlightColor === 'red') {
      return emacsTheme.red + 'aa'
    }
  }
  const missingColor = d3int.interpolate(emacsTheme.base1, emacsTheme.base2)(0.2)
  const borderColor = getBorderColor()
  const theme = useMemo(() => {
    console.log('ii')
    return {
      colors: {
        white: emacsTheme.bg,
        black: emacsTheme.fg,
        gray: {
          100: emacsTheme.base1,
          200: missingColor,
          300: emacsTheme.base2,
          400: emacsTheme.base3,
          500: emacsTheme.base4,
          600: emacsTheme.base5,
          700: emacsTheme.base6,
          800: emacsTheme.base7,
          900: emacsTheme.base8,
        },
        blue: {
          500: emacsTheme.blue,
        },
        teal: {
          500: emacsTheme.blue,
        },
        yellow: {
          500: emacsTheme.yellow,
        },
        orange: {
          500: emacsTheme.orange,
        },
        red: {
          500: emacsTheme.red,
        },
        green: {
          500: emacsTheme.green,
        },
        purple: {
          500: emacsTheme.violet,
        },
        pink: {
          500: emacsTheme.magenta,
        },
        cyan: {
          500: emacsTheme.cyan,
        },
        alt: {
          100: emacsTheme['bg-alt'],
          900: emacsTheme['fg-alt'],
        },
      },
      shadows: {
        outline: '0 0 0 3px ' + borderColor,
      },
      components: {
        Button: {
          variants: {
            outline: {
              border: '2px solid',
              borderColor: highlightColor + '.500',
              color: highlightColor + '.500',
            },
            ghost: {
              color: highlightColor + '.500',
            },
          },
        },
      },
    }
  }, [highlightColor, JSON.stringify(emacsTheme)])

  const extendedTheme = extendTheme(theme, withDefaultColorScheme({ colorScheme: highlightColor }))
  return <ChakraProvider theme={extendedTheme}>{children}</ChakraProvider>
}
export default MyApp
