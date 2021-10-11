import React from 'react'
import { NodeObject } from 'force-graph'

import { NodeById, NodeByCite, LinksByNodeId } from '../../pages'
import { Box, Flex } from '@chakra-ui/react'
import { UniOrg } from '../../util/uniorg'
import { Backlinks } from '../../components/Sidebar/Backlinks'
import { noteStyle } from './noteStyle'

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
  } = props
  return (
    <Box
      pr={8}
      overflow="scroll"
      height="100%"
      className="org"
      sx={{
        ...noteStyle,

        textAlign: justificationList[justification],
      }}
    >
      {previewNode?.id && (
        <Flex height="100%" flexDirection="column" justifyContent="space-between">
          <UniOrg
            {...{
              setPreviewNode,
              previewNode,
              nodeById,
              nodeByCite,
              setSidebarHighlightedNode,
              openContextMenu,
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
            }}
          />
        </Flex>
      )}
    </Box>
  )
}
