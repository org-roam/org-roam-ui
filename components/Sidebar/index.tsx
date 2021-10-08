import React, { useContext, useEffect, useState } from 'react'

import { Toolbar } from './Toolbar'
import { Note } from './Note'

import { Button, Slide, VStack, Flex, Heading, Box, IconButton } from '@chakra-ui/react'
import { Scrollbars } from 'react-custom-scrollbars-2'
import { ChevronLeftIcon, ChevronRightIcon, HamburgerIcon } from '@chakra-ui/icons'
import { BiFile } from 'react-icons/bi'

import { GraphData, NodeObject, LinkObject } from 'force-graph'
import { OrgRoamNode } from '../../api'
import { ThemeContext } from '../../util/themecontext'
import { LinksByNodeId, NodeByCite, NodeById } from '../../pages/index'

export interface SidebarProps {
  isOpen: boolean
  onClose: any
  onOpen: any
  nodeById: NodeById
  previewNode: NodeObject
  setPreviewNode: any
  linksByNodeId: LinksByNodeId
  nodeByCite: NodeByCite
  setSidebarHighlightedNode: any
}

const Sidebar = (props: SidebarProps) => {
  const {
    isOpen,
    onOpen,
    onClose,
    previewNode,
    setPreviewNode,
    nodeById,
    linksByNodeId,
    nodeByCite,
    setSidebarHighlightedNode,
  } = props

  const { highlightColor } = useContext(ThemeContext)
  const [previewRoamNode, setPreviewRoamNode] = useState<OrgRoamNode>()

  useEffect(() => {
    if (!previewNode?.id) {
      onClose()
      return
    }
    onOpen()
    setPreviewRoamNode(previewNode as OrgRoamNode)
  }, [previewNode?.id])

  const [justification, setJustification] = useState(1)
  const justificationList = ['justify', 'start', 'end', 'center']
  const [font, setFont] = useState('sans serif')
  const [indent, setIndent] = useState(0)
  //maybe want to close it when clicking outside, but not sure
  //const outsideClickRef = useRef();
  return (
    <Slide
      direction="right"
      in={isOpen}
      style={{ width: 'clamp(400px, 30%, 500px)' }}
      unmountOnExit
    >
      <Flex flexDirection="row" height="100%">
        <Box pl={2} color="black" bg="alt.100" w="100%" paddingBottom={15}>
          <Flex
            justifyContent="space-between"
            paddingTop={4}
            pl={4}
            pb={5}
            pr={3}
            alignItems="top"
            color="black"
          >
            <Flex alignItems="center" whiteSpace="nowrap" textOverflow="ellipsis" overflow="hidden">
              <BiFile />
              <Heading
                pl={2}
                whiteSpace="nowrap"
                textOverflow="ellipsis"
                overflow="hidden"
                lineHeight={1}
                size="sm"
                fontWeight={600}
                color={'gray.800'}
              >
                {previewRoamNode?.title}
              </Heading>
            </Flex>
            <IconButton
              // eslint-disable-next-line react/jsx-no-undef
              icon={<HamburgerIcon />}
              aria-label="Close file-viewer"
              variant="link"
              onClick={onClose}
            />
          </Flex>
          <Toolbar {...{ setJustification, setIndent, setFont, justification }} />
          <Scrollbars
            //autoHeight
            //autoHeightMax={600}
            autoHide
            renderThumbVertical={({ style, ...props }) => (
              <Box
                style={{
                  ...style,
                  borderRadius: 10,
                  backgroundColor: highlightColor,
                }}
                color="black"
                {...props}
              />
            )}
          >
            <VStack height="100%" alignItems="left" bg="alt.100" paddingLeft={4}>
              <Note
                {...{
                  setPreviewNode,
                  previewNode,
                  nodeById,
                  nodeByCite,
                  setSidebarHighlightedNode,
                  justification,
                  justificationList,
                  linksByNodeId,
                }}
              />
            </VStack>
          </Scrollbars>
        </Box>
      </Flex>
    </Slide>
  )
}

export default Sidebar
