import React from 'react'
import { NodeObject } from 'force-graph'

import { NodeById, NodeByCite, LinksByNodeId } from '../../pages'
import { Box, Flex } from '@chakra-ui/react'
import { UniOrg } from '../../util/uniorg'
import { Backlinks } from '../../components/Sidebar/Backlinks'
import { defaultNoteStyle, viewerNoteStyle, outlineNoteStyle } from './noteStyle'

export interface NoteProps {
  setPreviewNode: any
  previewNode: NodeObject
  nodeById: NodeById
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: any
  justification: number
  justificationList: string[]
  linksByNodeId: LinksByNodeId
  openContextMenu: any
  outline: boolean
}

export const Note = (props: NoteProps) => {
  const {
    setPreviewNode,
    justificationList,
    justification,
    previewNode,
    nodeById,
    nodeByCite,
    setSidebarHighlightedNode,
    linksByNodeId,
    openContextMenu,
    outline,
  } = props
  const extraStyle = outline ? outlineNoteStyle : viewerNoteStyle
  return (
    <Box
      pr={8}
      pt={2}
      height="100%"
      className="org"
      sx={{
        ...defaultNoteStyle,
        ...extraStyle,
        textAlign: justificationList[justification],
      }}
    >
      {previewNode?.id && (
        <Flex
          className="wrapClass"
          height="100%"
          flexDirection="column"
          justifyContent="space-between"
        >
          <UniOrg
            {...{
              setPreviewNode,
              previewNode,
              nodeById,
              nodeByCite,
              setSidebarHighlightedNode,
              openContextMenu,
              outline,
            }}
          />
          <Backlinks
            {...{
              setPreviewNode,
              previewNode,
              nodeById,
              linksByNodeId,
              nodeByCite,
              setSidebarHighlightedNode,
              openContextMenu,
              outline,
            }}
          />
        </Flex>
      )}
    </Box>
  )
}
