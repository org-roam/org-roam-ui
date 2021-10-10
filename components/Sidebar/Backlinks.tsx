import { LinksByNodeId, NodeByCite, NodeById } from '../../pages/index'

import { GraphData, NodeObject, LinkObject } from 'force-graph'

import { normalizeLinkEnds } from '../../pages/index'
import { VStack, Box, Button, Heading, StackDivider } from '@chakra-ui/react'
import React from 'react'
import { ProcessedOrg } from '../../util/processOrg'

export interface BacklinksProps {
  previewNode: any
  setPreviewNode: any
  nodeById: NodeById
  linksByNodeId: LinksByNodeId
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: OrgRoamNode
  openContextMenu: any
}

import { PreviewLink } from './Link'
import { OrgRoamNode } from '../../api'

export const Backlinks = (props: BacklinksProps) => {
  const {
    previewNode,
    setPreviewNode,
    setSidebarHighlightedNode,
    nodeById,
    linksByNodeId,
    nodeByCite,
    openContextMenu,
  } = props
  const links = linksByNodeId[previewNode?.id] ?? []

  const backLinks = links
    .filter((link: LinkObject) => {
      const [source, target] = normalizeLinkEnds(link)
      return source !== previewNode?.id
    })
    .map((l) => l.source)

  return (
    <Box>
      <Heading pt={4}>{`Backlinks (${backLinks.length})`}</Heading>
      <VStack
        pt={2}
        spacing={3}
        alignItems="start"
        divider={<StackDivider borderColor="gray.500" />}
        align="stretch"
        color="gray.800"
      >
        {previewNode?.id &&
          backLinks.map((link) => {
            const title = nodeById[link as string]?.title ?? ''
            return (
              <Box overflow="hidden" p={3} bg="gray.300" width="100%" key={link}>
                <PreviewLink
                  nodeByCite={nodeByCite}
                  setSidebarHighlightedNode={setSidebarHighlightedNode}
                  href={`id:${link as string}`}
                  nodeById={nodeById}
                  setPreviewNode={setPreviewNode}
                  openContextMenu={openContextMenu}
                >
                  {nodeById[link as string]?.title}
                </PreviewLink>
              </Box>
            )
          })}
      </VStack>
    </Box>
  )
}
