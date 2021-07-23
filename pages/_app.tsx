import '../styles/globals.css'
import type { AppProps } from 'next/app'
import { ChakraProvider } from '@chakra-ui/react'
import { useEffect, useState, useMemo } from 'react'
import { extendTheme } from '@chakra-ui/react'
//import hexRgb from 'hex-rgb'

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
  const [emacsTheme, setEmacsTheme] = useState(initialTheme)
  useEffect(() => {
    const trackTheme = new EventSource('http://127.0.0.1:35901/theme')
    trackTheme.addEventListener('message', (e) => {
      const themeData = JSON.parse(e.data)
      setEmacsTheme(themeData)
      console.log(themeData)
      console.log(themeData['fg-alt'])
    })
  }, [])

  const borderColor = emacsTheme.violet + 'aa'
  const theme =
    //useMemo(() => {
    //console.log("theme change")
    extendTheme({
      colors: {
        white: emacsTheme.bg,
        black: emacsTheme.fg,
        gray: {
          200: emacsTheme.base1,
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
    })
  // }, [emacsTheme.base1])

  return (
    <ChakraProvider theme={theme}>
      <Component {...pageProps} />
    </ChakraProvider>
  )
}
export default MyApp
