import React, { useContext, useEffect, useState } from 'react'

import { UniOrg } from '../../util/uniorg'
import { getOrgText } from '../../util/webSocketFunctions'

import {
  Button,
  Slide,
  VStack,
  Flex,
  Heading,
  Box,
  CloseButton,
  Text,
  Drawer,
  DrawerOverlay,
  DrawerHeader,
  DrawerBody,
  DrawerCloseButton,
  DrawerContent,
  DrawerFooter,
  IconButton,
} from '@chakra-ui/react'
import { Scrollbars } from 'react-custom-scrollbars-2'
import { ChevronLeftIcon, ChevronRightIcon } from '@chakra-ui/icons'

import { GraphData, NodeObject, LinkObject } from 'force-graph'
import { OrgRoamNode } from '../../api'
import { ThemeContext } from '../../util/themecontext'

export interface SidebarProps {
  isOpen: boolean
  onClose: any
  //nodeById: any
  previewNode: NodeObject
  orgText: string
}

const Sidebar = (props: SidebarProps) => {
  const { isOpen, onClose, previewNode, orgText } = props

  const { highlightColor } = useContext(ThemeContext)
  const [previewRoamNode, setPreviewRoamNode] = useState<OrgRoamNode>()

  useEffect(() => {
    if (!previewNode) {
      return
    }

    setPreviewRoamNode(previewNode as OrgRoamNode)
  }, [previewNode])

  //maybe want to close it when clicking outside, but not sure
  //const outsideClickRef = useRef();
  return (
    <Slide direction="right" in={isOpen} style={{ zIndex: 200, width: 500 }} unmountOnExit>
      <Flex flexDirection="row" height="100%">
        <IconButton
          icon={<ChevronRightIcon height={30} />}
          colorScheme="white"
          aria-label="Close file-viewer"
          height={100}
          variant="ghost"
          marginRight={-2}
          bg="alt.100"
          onClick={onClose}
          marginTop={20}
        />
        <Box
          color="gray.800"
          bg="alt.100"
          boxShadow="xl"
          w={500}
          height="98%"
          position="relative"
          zIndex="overlay"
          marginTop={10}
          paddingBottom={15}
          borderRadius="xl"
          right={-2}
        >
          <Flex
            justifyContent="space-between"
            padding={4}
            paddingTop={10}
            paddingLeft={10}
            width="80%"
            alignItems="center"
            color="black"
          >
            <Heading size="md">{previewRoamNode?.title}</Heading>
          </Flex>
          <Scrollbars
            //autoHeight
            //autoHeightMax={600}
            autoHide
            renderThumbVertical={({ style, ...props }) => (
              <Box
                {...props}
                style={{
                  ...style,
                  borderRadius: 10,
                }}
                bg={highlightColor}
              />
            )}
          >
            <VStack alignItems="left" bg="alt.100" paddingLeft={10} paddingRight={10}>
              <Box
                className="org"
                sx={{
                  h1: { display: 'none' },
                  h2: {
                    fontSize: '20',
                    fontWeight: 'bold !important',
                    marginBottom: '1em',
                    color: 'black',
                  },
                  h3: {
                    fontSize: '18',
                    fontWeight: '600 !important',
                    marginBottom: '.5em',
                  },
                  h4: {
                    fontSize: '16',
                    fontWeight: '500 !important',
                    marginBottom: '.25em',
                    fontStyle: 'italic',
                  },
                  a: {
                    color: highlightColor,
                    pointerEvents: 'none',
                  },
                  ol: {
                    paddingLeft: '5',
                  },
                  ul: {
                    paddingLeft: '5',
                  },
                  p: {
                    paddingBottom: '.5em',
                  },
                  '.katex-html': { visibility: 'hidden', width: '0px', position: 'absolute' },
                  '#content': { textAlign: 'justify' },
                  '.title': {
                    textAlign: 'center',
                    marginBottom: '.2em',
                  },
                  '.subtitle': {
                    textAlign: 'center',
                    fontSize: 'medium',
                    fontWeight: 'bold',
                    marginTop: 0,
                  },
                  '.todo': { fontFamily: 'monospace', color: 'red' },
                  '.equationContainer': {
                    display: 'table',
                    textAlign: 'center',
                    width: '100%',
                  },
                  '.equation': {
                    verticalAlign: 'middle',
                  },
                  '.equation-label': {
                    display: 'tableCell',
                    textAlign: 'right',
                    verticalAlign: 'middle',
                  },
                  '.inlinetask': {
                    padding: '10px',
                    border: '2px solid gray',
                    margin: '10px',
                    background: '#ffffcc',
                  },
                  '#org-div-home-and-up': {
                    textAlign: 'right',
                    fontSize: '70 % ',
                    whiteSpace: 'nowrap',
                  },
                  textarea: { overflowX: 'auto' },
                  '.linenr': { fontSize: 'smaller' },
                  '.code-highlighted': { backgroundColor: '#ffff00' },
                  '.org-info-js_info-navigation': { borderStyle: 'none' },
                  '#org-info-js_console-label': {
                    fontSize: '10px',
                    fontWeight: 'bold',
                    whiteSpace: 'nowrap',
                  },
                  '.org-info-js_search-highlight': {
                    backgroundColor: '#ffff00',
                    color: '#000000',
                    fontWeight: 'bold',
                  },
                  '.org-svg': { width: '90%' },
                  '.done': { fontFamily: 'monospace', color: 'green' },
                  '.priority': { fontFamily: 'monospace', color: 'orange' },
                  '.tag': {
                    backgroundColor: '#eee',
                    fontFamily: 'monospace',
                    padding: '2px',
                    fontSize: '80%',
                    fontWeight: 'normal',
                  },
                  '.timestamp': { color: '#bebebe' },
                  '.timestamp-kwd': { color: '#5f9ea0' },
                  '.org-right': { marginLeft: 'auto', marginRight: '0px', textAlign: 'right' },
                  '.org-left': { marginLeft: '0px', marginRight: 'auto', textAlign: 'left' },
                  '.org-center': { marginLeft: 'auto', marginRight: 'auto', textAlign: 'center' },
                  '.underline': { textDecoration: 'underline' },
                  '#postamble p': { fontSize: '90%', margin: '.2em' },
                  '#preamble p': { fontSize: '90%', margin: '.2em' },
                  'p.verse': { marginLeft: '3%' },
                  pre: {
                    border: '1px solid #e6e6e6',
                    borderRadius: '3px',
                    backgroundColor: '#f2f2f2',
                    padding: '8pt',
                    fontFamily: 'monospace',
                    overflow: 'auto',
                    margin: '1.2em',
                  },
                  'pre.src': {
                    position: 'relative',
                    overflow: 'auto',
                  },
                  'pre.src:before': {
                    display: 'none',
                    position: 'absolute',
                    top: '-8px',
                    right: '12px',
                    padding: '3px',
                    color: '#555',
                    backgroundColor: '#f2f2f299',
                  },
                  'caption.t-above': { captionSide: 'top' },
                  'caption.t-bottom': { captionSide: 'bottom' },
                  'th.org-right': { textAlign: 'center' },
                  'th.org-left': { textAlign: 'center' },
                  'th.org-center': { textAlign: 'center' },
                  'td.org-right': { textAlign: 'right' },
                  'td.org-left': { textAlign: 'left' },
                  'td.org-center': { textAlign: 'center' },
                  '.footpara': { display: 'inline' },
                  '.footdef': { marginBottom: '1em' },
                  '.figure': { padding: '1em' },
                  '.figure p': { textAlign: 'center' },
                }}
              >
                <UniOrg orgText={orgText} />
              </Box>
            </VStack>
          </Scrollbars>
        </Box>
      </Flex>
    </Slide>
  )
}

export default Sidebar

/* <Box marginLeft="auto" zIndex={5000} bg="alt.100" maxHeight="100%" width= "30%" padding={8} borderRadius={2}>
   <Heading size="sm">{previewNode.title}</Heading>
</Box> */
