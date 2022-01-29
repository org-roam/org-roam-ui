/* eslint-disable react/display-name */
import {
  Box,
  Button,
  Link,
  Popover,
  PopoverArrow,
  PopoverBody,
  PopoverCloseButton,
  PopoverContent,
  PopoverHeader,
  PopoverTrigger,
  Portal,
  Text,
  useTheme,
} from '@chakra-ui/react'
import React, { ReactElement, useContext, useEffect, useMemo, useState } from 'react'

import { ProcessedOrg } from '../../util/processOrg'
import unified from 'unified'
//import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
//import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import 'katex/dist/katex.css'
import rehype2react from 'rehype-react'
import { ThemeContext } from '../../util/themecontext'
import { LinksByNodeId, NodeByCite, NodeById } from '../../pages'

export interface LinkProps {
  href: any
  children: any
  previewNode?: any
  setPreviewNode: any
  setSidebarHighlightedNode: any
  nodeByCite: NodeByCite
  nodeById: NodeById
  openContextMenu: any
  outline: boolean
  linksByNodeId: LinksByNodeId
  isWiki?: boolean
  noUnderline?: boolean
  attachDir: string
  macros: { [key: string]: string }
}

export interface NodeLinkProps {
  setPreviewNode: any
  nodeById: NodeById
  nodeByCite: NodeByCite
  href: any
  children: any
  setSidebarHighlightedNode: any
  openContextMenu: any
  isWiki?: boolean
  noUnderline?: boolean
  id?: string
}
export interface NormalLinkProps {
  href: string
  children: string
}

import { hexToRGBA, getThemeColor } from '../../pages/index'
import { defaultNoteStyle, viewerNoteStyle, outlineNoteStyle } from './noteStyle'
import { OrgImage } from './OrgImage'
import { Scrollbars } from 'react-custom-scrollbars-2'
import { ExternalLinkIcon } from '@chakra-ui/icons'
import { Section } from './Section'
import { OrgRoamLink } from '../../api'

export const NodeLink = (props: NodeLinkProps) => {
  const {
    noUnderline,
    id,
    setSidebarHighlightedNode,
    setPreviewNode,
    nodeById,
    openContextMenu,
    href,
    children,
    isWiki,
  } = props
  const { highlightColor } = useContext(ThemeContext)

  const theme = useTheme()
  const coolHighlightColor = getThemeColor(highlightColor, theme)
  const type = href.replaceAll(/(.*?)\:?.*/g, '$1')
  const uri = href.replaceAll(/.*?\:(.*)/g, '$1')
  const ID = id ?? uri
  const linkText = isWiki ? `[[${children}]]` : children
  return (
    <Text
      as="a"
      onMouseEnter={() => setSidebarHighlightedNode(nodeById[ID])}
      onMouseLeave={() => setSidebarHighlightedNode({})}
      tabIndex={0}
      display="inline"
      overflow="hidden"
      fontWeight={500}
      color={highlightColor}
      textDecoration={noUnderline ? undefined : 'underline'}
      onContextMenu={(e) => {
        e.preventDefault()
        openContextMenu(nodeById[uri], e)
      }}
      onClick={() => setPreviewNode(nodeById[uri])}
      // TODO  don't hardcode the opacitycolor
      _hover={{ textDecoration: 'none', cursor: 'pointer', bgColor: coolHighlightColor + '22' }}
      _focus={{ outlineColor: highlightColor }}
    >
      {linkText}
    </Text>
  )
}

export const NormalLink = (props: NormalLinkProps) => {
  const { href, children } = props
  const { highlightColor } = useContext(ThemeContext)
  return (
    <Link color={highlightColor} isExternal href={href}>
      {children}
      <ExternalLinkIcon mx="1px" pb="2px" />
    </Link>
  )
}

export const PreviewLink = (props: LinkProps) => {
  const {
    href,
    children,
    nodeById,
    setSidebarHighlightedNode,
    previewNode,
    setPreviewNode,
    nodeByCite,
    openContextMenu,
    outline,
    noUnderline,
    linksByNodeId,
    isWiki,
    macros,
    attachDir,
  } = props
  // TODO figure out how to properly type this
  // see https://github.com/rehypejs/rehype-react/issues/25
  const [orgText, setOrgText] = useState<any>(null)
  const [hover, setHover] = useState(false)
  const type = href.replaceAll(/(.*?)\:.*/g, '$1')

  const extraNoteStyle = outline ? outlineNoteStyle : viewerNoteStyle
  console.log(previewNode)
  const getText = () => {
    fetch(`http://localhost:35901/node/${id}`)
      .then((res) => {
        return res.text()
      })
      .then((res) => {
        if (res !== 'error') {
          setOrgText(res)
          return
        }
      })
      .catch((e) => {
        console.log(e)
        return 'Could not fetch the text for some reason, sorry!\n\n This can happen because you have an id with forward slashes (/) in it.'
      })
  }

  useEffect(() => {
    if (type.replaceAll(/(http)?.*/g, '$1')) {
      return
    }
    if (!!orgText) {
      return
    }
    if (!hover) {
      return
    }
    getText()
  }, [hover, orgText])

  if (!type) {
    return <Text color="gray.700">{children}</Text>
  }

  if (type.replaceAll(/(http)?.*/g, '$1')) {
    return <NormalLink href={href}>{children}</NormalLink>
  }

  const uri = href.replaceAll(/.*?\:(.*)/g, '$1')
  const getId = (type: string, uri: string) => {
    if (type === 'id') {
      return uri
    }

    if (type.includes('cite')) {
      const node = nodeByCite[uri] ?? false
      if (!node) {
        return ''
      }
      if (node?.properties.FILELESS) {
        return ''
      }
      return node?.id
    }
    return ''
  }

  const id = getId(type, uri)
  const file = encodeURIComponent(encodeURIComponent(nodeById[id]?.file as string))

  if (id) {
    return (
      <>
        <Popover gutter={12} trigger="hover" placement="top-start">
          <PopoverTrigger>
            <Box
              display="inline"
              onMouseEnter={() => setHover(true)}
              onMouseLeave={() => setHover(false)}
            >
              <NodeLink
                key={nodeById[id]?.title ?? id}
                {...{
                  id,
                  setSidebarHighlightedNode,
                  setPreviewNode,
                  nodeById,
                  href,
                  children,
                  nodeByCite,
                  openContextMenu,
                  noUnderline,
                  isWiki,
                }}
              />
            </Box>
          </PopoverTrigger>
          <Portal>
            <PopoverContent
              transform="scale(1)"
              key={nodeById[id]?.title ?? id}
              boxShadow="xl"
              position="relative"
              zIndex="tooltip"
              onMouseEnter={() => {
                setSidebarHighlightedNode(nodeById[id] ?? {})
              }}
              onMouseLeave={() => {
                setSidebarHighlightedNode({})
              }}
            >
              <PopoverArrow />
              <PopoverBody
                pb={5}
                fontSize="xs"
                position="relative"
                zIndex="tooltip"
                transform="scale(1)"
                width="100%"
              >
                <Scrollbars
                  autoHeight
                  autoHeightMax={300}
                  autoHide
                  renderThumbVertical={({ style, ...props }) => (
                    <Box
                      style={{
                        ...style,
                        borderRadius: 0,
                        // backgroundColor: highlightColor,
                      }}
                      //color="alt.100"
                      {...props}
                    />
                  )}
                >
                  <Box
                    w="100%"
                    color="black"
                    px={3}
                    sx={{ ...defaultNoteStyle, ...extraNoteStyle }}
                    //overflowY="scroll"
                  >
                    <ProcessedOrg
                      previewText={orgText}
                      {...{
                        nodeById,
                        setSidebarHighlightedNode,
                        setPreviewNode,
                        nodeByCite,
                        openContextMenu,
                        outline,
                        linksByNodeId,
                        macros,
                        attachDir,
                      }}
                      previewNode={nodeById[id]!}
                      collapse={false}
                    />
                  </Box>
                </Scrollbars>
              </PopoverBody>
            </PopoverContent>
          </Portal>
        </Popover>
      </>
    )
  }
  return (
    <Text as="span" display="inline" className={href} color="base.700" cursor="not-allowed">
      {children}
    </Text>
  )
}
