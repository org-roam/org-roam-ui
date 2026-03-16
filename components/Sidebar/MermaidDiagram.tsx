import React, { useEffect, useRef, useState } from 'react'
import { Box } from '@chakra-ui/react'

let mermaidId = 0
let mermaidLoaded: Promise<any> | null = null

function loadMermaid(): Promise<any> {
  if (mermaidLoaded) return mermaidLoaded
  mermaidLoaded = new Promise((resolve, reject) => {
    if (typeof window !== 'undefined' && (window as any).mermaid) {
      resolve((window as any).mermaid)
      return
    }
    const script = document.createElement('script')
    script.src = 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.min.js'
    script.onload = () => {
      const m = (window as any).mermaid
      m.initialize({ startOnLoad: false, theme: 'dark', securityLevel: 'loose' })
      resolve(m)
    }
    script.onerror = reject
    document.head.appendChild(script)
  })
  return mermaidLoaded
}

export interface MermaidDiagramProps {
  code: string
}

export const MermaidDiagram = ({ code }: MermaidDiagramProps) => {
  const [svg, setSvg] = useState<string>('')
  const [error, setError] = useState<string>('')
  const [id] = useState(() => `mermaid-${mermaidId++}`)

  useEffect(() => {
    let cancelled = false
    loadMermaid()
      .then(async (mermaid) => {
        try {
          const { svg: rendered } = await mermaid.render(id, code)
          if (!cancelled) {
            setSvg(rendered)
            setError('')
          }
        } catch (e: any) {
          if (!cancelled) {
            setError(e?.message || 'Failed to render diagram')
            setSvg('')
          }
        }
      })
      .catch((e: any) => {
        if (!cancelled) {
          setError('Failed to load Mermaid library')
        }
      })
    return () => {
      cancelled = true
    }
  }, [code, id])

  if (error) {
    return (
      <Box
        p={3}
        my={3}
        bg="red.900"
        color="red.200"
        borderRadius="md"
        fontFamily="monospace"
        fontSize="sm"
      >
        Mermaid error: {error}
      </Box>
    )
  }

  if (!svg) {
    return (
      <Box p={3} my={3} color="gray.400" fontSize="sm">
        Rendering diagram...
      </Box>
    )
  }

  return (
    <Box
      my={3}
      p={2}
      borderRadius="md"
      overflow="auto"
      sx={{ '& svg': { maxWidth: '100%' } }}
      dangerouslySetInnerHTML={{ __html: svg }}
    />
  )
}
