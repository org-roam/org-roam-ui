import '../styles/globals.css'
import type { AppProps } from 'next/app'
import { ChakraProvider, extendTheme, withDefaultColorScheme } from '@chakra-ui/react'
import { useEffect, useState, useMemo, useContext, useReducer } from 'react'
import * as d3int from 'd3-interpolate'

import { ThemeContext } from '../util/themecontext'
import { usePersistantState } from '../util/persistant-state'
import { themes } from '../components/themes'

function MyApp({ Component, pageProps }: AppProps) {
  type Theme = [string, { [color: string]: string }]
  const initialTheme: Theme = ['one-vibrant', themes['one-vibrant']]
  const [isInitialized, setIsInitialized] = useState(false)

  const [emacsTheme, setEmacsTheme] = useState<Theme>(initialTheme)
  const [highlightColor, setHighlightColor] = useState('purple.500')

  useEffect(() => {
    if (isInitialized) {
      localStorage.setItem('colorTheme', JSON.stringify(emacsTheme))
    }
    console.log(emacsTheme)
  }, [emacsTheme])

  useEffect(() => {
    if (isInitialized) {
      localStorage.setItem('highlightColor', JSON.stringify(highlightColor))
    }
  }, [highlightColor])

  useEffect(() => {
    setEmacsTheme(
      JSON.parse(localStorage.getItem('colorTheme') ?? JSON.stringify(initialTheme)) ??
        initialTheme,
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
  type Theme = { [color: string]: string }
  const themeColors: Theme = emacsTheme[1] as Theme
  // yeah it's annoying, should put this someplace more sensible
  const getBorderColor = () => {
    if (highlightColor === 'purple.500') {
      return `${themeColors['violet']}aa`
    }
    if (highlightColor === 'pink.500') {
      return `${themeColors['magenta']}aa`
    }
    if (highlightColor === 'blue.500') {
      return `${themeColors['blue']}aa`
    }
    if (highlightColor === 'cyan.500') {
      return `${themeColors['cyan']}aa`
    }
    if (highlightColor === 'green.500') {
      return `${themeColors['green']}aa`
    }
    if (highlightColor === 'yellow.500') {
      return `${themeColors['yellow']}aa`
    }
    if (highlightColor === 'orange.500') {
      return `${themeColors['orange']}aa`
    }
    if (highlightColor === 'red.500') {
      return `${themeColors['red']}aa`
    }
  }
  const missingColor = d3int.interpolate(themeColors['base1'], themeColors['base2'])(0.2)
  const borderColor = getBorderColor()
  const theme = useMemo(() => {
    return {
      colors: {
        white: themeColors['bg'],
        black: themeColors['fg'],
        gray: {
          100: themeColors['base1'],
          200: missingColor,
          300: themeColors['base2'],
          400: themeColors['base3'],
          500: themeColors['base4'],
          600: themeColors['base5'],
          700: themeColors['base6'],
          800: themeColors['base7'],
          900: themeColors['base8'],
        },
        blue: {
          500: themeColors['blue'],
        },
        teal: {
          500: themeColors['blue'],
        },
        yellow: {
          500: themeColors['yellow'],
        },
        orange: {
          500: themeColors['orange'],
        },
        red: {
          500: themeColors['red'],
        },
        green: {
          500: themeColors['green'],
        },
        purple: {
          500: themeColors['violet'],
        },
        pink: {
          500: themeColors['magenta'],
        },
        cyan: {
          500: themeColors['cyan'],
        },
        alt: {
          100: themeColors['bg-alt'],
          900: themeColors['fg-alt'],
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
              borderColor: highlightColor,
              color: highlightColor,
            },
            ghost: {
              color: highlightColor,
              _hover: { bg: `inherit`, border: '1px solid', borderColor: highlightColor },
              _active: { color: `inherit`, bg: highlightColor },
            },
          },
        },
        Accordion: {
          baseStyle: {
            container: {
              marginTop: '10px',
              borderWidth: '0px',
              _last: {
                borderWidth: '0px',
              },
            },
            panel: {
              marginRight: '10px',
            },
          },
        },
        Slider: {
          baseStyle: (props: any) => ({
            thumb: {
              backgroundColor: highlightColor,
            },
            filledTrack: {
              backgroundColor: 'gray.200',
            },
          }),
        },
      },
    }
  }, [highlightColor, JSON.stringify(emacsTheme)])

  const extendedTheme = extendTheme(
    theme,
    withDefaultColorScheme({ colorScheme: highlightColor.split('.')[0] }),
  )
  return <ChakraProvider theme={extendedTheme}>{children}</ChakraProvider>
}
export default MyApp
