import unified from 'unified'
//import createStream from 'unified-stream'
import uniorgParse from 'uniorg-parse'
import uniorg2rehype from 'uniorg-rehype'
import uniorgSlug from 'uniorg-slug'
import extractKeywords from 'uniorg-extract-keywords'
import attachments from 'uniorg-attach'
// rehypeHighlight does not have any types
// add error thing here
// import highlight from 'rehype-highlight'
import katex from 'rehype-katex'
import 'katex/dist/katex.css'
import rehype2react from 'rehype-react'

import { PreviewLink } from '../components/Sidebar/Link'
import { NodeByCite, NodeById } from '../pages'
import React, { createContext, ReactNode, useMemo } from 'react'
import { OrgImage } from '../components/Sidebar/OrgImage'
import { Section } from '../components/Sidebar/Section'
import { NoteContext } from './NoteContext'

export interface ProcessedOrgProps {
  nodeById: NodeById
  previewNode: any
  setPreviewNode: any
  previewText: any
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: any
  openContextMenu: any
  outline: boolean
  collapse: boolean
}

export const ProcessedOrg = (props: ProcessedOrgProps) => {
  const {
    nodeById,
    setSidebarHighlightedNode,
    setPreviewNode,
    previewText,
    nodeByCite,
    previewNode,
    openContextMenu,
    outline,
    collapse,
  } = props

  const processor = unified()
    .use(uniorgParse)
    .use(extractKeywords)
    .use(attachments)
    .use(uniorgSlug)
    .use(uniorg2rehype, { useSections: true })
    //.data('settings', { fragment: true })
    // .use(highlight)
    .use(katex)
    .use(rehype2react, {
      createElement: React.createElement,
      // eslint-disable-next-line react/display-name
      components: {
        a: ({ children, href }) => {
          return (
            <PreviewLink
              nodeByCite={nodeByCite}
              setSidebarHighlightedNode={setSidebarHighlightedNode}
              href={`${href as string}`}
              nodeById={nodeById}
              setPreviewNode={setPreviewNode}
              openContextMenu={openContextMenu}
              outline={outline}
            >
              {children}
            </PreviewLink>
          )
        },
        img: ({ src }) => {
          return <OrgImage src={src as string} file={previewNode?.file} />
        },
        section: ({ children, className }) => (
          <Section {...{ outline, collapse }} className={className as string}>
            {children}
          </Section>
        ),
        p: ({ children }) => {
          return <p lang="en">{children as ReactNode}</p>
        },
      },
    })

  const text = useMemo(() => processor.processSync(previewText).result, [previewText])
  return <NoteContext.Provider value={{ collapse, outline }}>{text}</NoteContext.Provider>
}
