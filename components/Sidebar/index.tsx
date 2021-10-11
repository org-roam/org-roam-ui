import React, { useContext, useEffect, useRef, useState } from 'react'

import { Toolbar } from './Toolbar'
import { TagBar } from './TagBar'
import { Note } from './Note'

import {
  Button,
  Slide,
  VStack,
  Flex,
  Heading,
  Box,
  IconButton,
  Tooltip,
  HStack,
  TagLabel,
  Tag,
  TagRightIcon,
} from '@chakra-ui/react'
import { Collapse } from './Collapse'
import { Scrollbars } from 'react-custom-scrollbars-2'
import {
  ChevronLeftIcon,
  ChevronRightIcon,
  CloseIcon,
  HamburgerIcon,
  ViewIcon,
  ViewOffIcon,
} from '@chakra-ui/icons'
import { BiDotsVerticalRounded, BiFile, BiNetworkChart } from 'react-icons/bi'
import { BsReverseLayoutSidebarInsetReverse } from 'react-icons/bs'

import { GraphData, NodeObject, LinkObject } from 'force-graph'
import { OrgRoamNode } from '../../api'
import { ThemeContext } from '../../util/themecontext'
import { LinksByNodeId, NodeByCite, NodeById, Scope } from '../../pages/index'
import { Resizable } from 're-resizable'
import { usePersistantState } from '../../util/persistant-state'
import { initialFilter, TagColors } from '../config'

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
  canUndo: any
  canRedo: any
  resetPreviewNode: any
  previousPreviewNode: any
  nextPreviewNode: any
  openContextMenu: any
  scope: Scope
  setScope: any
  windowWidth: number
  filter: typeof initialFilter
  setFilter: any
  tagColors: TagColors
  setTagColors: any
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
    canUndo,
    canRedo,
    resetPreviewNode,
    previousPreviewNode,
    nextPreviewNode,
    openContextMenu,
    scope,
    setScope,
    windowWidth,
    filter,
    setFilter,
    tagColors,
    setTagColors,
  } = props

  const { highlightColor } = useContext(ThemeContext)
  const [previewRoamNode, setPreviewRoamNode] = useState<OrgRoamNode>()
  const [sidebarWidth, setSidebarWidth] = usePersistantState<number>('sidebarWidth', 400)

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
    <Collapse
      animateOpacity={false}
      dimension="width"
      in={isOpen}
      //style={{ position: 'relative' }}
      unmountOnExit
      startingSize={0}
      style={{ height: '100vh' }}
    >
      <Resizable
        size={{ height: '100vh', width: sidebarWidth }}
        onResizeStop={(e, direction, ref, d) => {
          setSidebarWidth((curr: number) => curr + d.width)
        }}
        enable={{
          top: false,
          right: false,
          bottom: false,
          left: true,
          topRight: false,
          bottomRight: false,
          bottomLeft: false,
          topLeft: false,
        }}
        minWidth="220px"
        maxWidth={windowWidth - 200}
      >
        <Flex flexDir="column" h="100vh" pl={2} color="black" bg="alt.100" width="100%">
          <Flex
            //whiteSpace="nowrap"
            // overflow="hidden"
            // textOverflow="ellipsis"
            pl={4}
            alignItems="center"
            color="black"
            width="100%"
          >
            <Flex flexShrink={0}>
              <BiFile
                onContextMenu={(e) => {
                  e.preventDefault()
                  openContextMenu(previewNode, e)
                }}
              />
            </Flex>
            <Flex
              whiteSpace="nowrap"
              textOverflow="ellipsis"
              overflow="hidden"
              onContextMenu={(e) => {
                e.preventDefault()
                openContextMenu(previewNode, e)
              }}
            >
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
            <Flex flexDir="row" ml="auto">
              <IconButton
                // eslint-disable-next-line react/jsx-no-undef
                m={1}
                icon={<BiDotsVerticalRounded />}
                aria-label="Options"
                variant="subtle"
                onClick={(e) => {
                  openContextMenu(previewNode, e, {
                    left: undefined,
                    top: 12,
                    right: -windowWidth + 20,
                    bottom: undefined,
                  })
                }}
              />
            </Flex>
          </Flex>
          <Toolbar
            {...{
              setJustification,
              setIndent,
              setFont,
              justification,
              setPreviewNode,
              canUndo,
              canRedo,
              resetPreviewNode,
              previousPreviewNode,
              nextPreviewNode,
            }}
          />
          <Scrollbars
            //autoHeight
            //autoHeightMax={600}
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
            <VStack flexGrow={1} overflowY="scroll" alignItems="left" bg="alt.100" paddingLeft={4}>
              <TagBar
                {...{ filter, setFilter, tagColors, setTagColors, openContextMenu, previewNode }}
              />
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
                  openContextMenu,
                }}
              />
            </VStack>
          </Scrollbars>
        </Flex>
      </Resizable>
    </Collapse>
  )
}

export default Sidebar
