import {
  Box,
  Button,
  Popover,
  PopoverArrow,
  PopoverBody,
  PopoverCloseButton,
  PopoverContent,
  PopoverHeader,
  PopoverTrigger,
  Portal,
  Text,
} from '@chakra-ui/react'
import React, { useState } from 'react'
import UniOrg from '../../util/uniorg'

import unified from 'unified'
//import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
//import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import 'katex/dist/katex.css'
import rehype2react from 'rehype-react'

export interface LinkProps {
  href: string
  children: any
  testProp: string
  getText: any
  previewNode?: any
  setPreviewNode: any
}

export const PreviewLink = (props: any) => {
  const { href, children, nodeById, getText, previewNode, setPreviewNode } = props
  const [previewText, setPreviewText] = useState('')
  const [whatever, type, uri] = [...href.matchAll(/(.*?)\:(.*)/g)][0]

  const processor = unified().use(uniorgParse).use(uniorg2rehype).use(katex).use(rehype2react, {
    createElement: React.createElement,
    // eslint-disable-next-line react/display-name
  })

  type === 'id' && getText(uri, setPreviewText)

  return (
    <>
      <Popover trigger="hover" isLazy position="relative" zIndex="tooltip">
        <PopoverTrigger>
          <Button size="sm" onClick={() => setPreviewNode(nodeById[uri])} variant="link">
            {children}
          </Button>
        </PopoverTrigger>
        <Portal zIndex={100000} position="relative">
          <PopoverContent boxShadow="xl" position="relative" zIndex="tooltip">
            <PopoverHeader pl={5} fontSize="sm" zIndex="tooltip" fontWeight="semibold">
              {children}
            </PopoverHeader>
            <PopoverArrow zIndex={10000} />
            <PopoverCloseButton zIndex={10000} />
            <PopoverBody
              pb={5}
              fontSize="xs"
              px={5}
              position="relative"
              zIndex="tooltip"
              maxHeight={300}
              overflow="scroll"
            >
              {uri && <Box>{processor.processSync(previewText).result}</Box>}
            </PopoverBody>
          </PopoverContent>
        </Portal>
      </Popover>
    </>
  )
}
