import React, { useContext, useEffect, useState } from 'react'

import { UniOrg } from '../../util/uniorg'
import { Backlinks } from './Backlinks'
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
import { ChevronLeftIcon, ChevronRightIcon, HamburgerIcon } from '@chakra-ui/icons'
import {
  BiFont,
  BiAlignJustify,
  BiAlignLeft,
  BiAlignMiddle,
  BiAlignRight,
  BiRightIndent,
} from 'react-icons/bi'

import { GraphData, NodeObject, LinkObject } from 'force-graph'
import { OrgRoamNode } from '../../api'
import { ThemeContext } from '../../util/themecontext'
import { LinksByNodeId, NodeById } from '../../pages/index'

export interface SidebarProps {
  isOpen: boolean
  onClose: any
  onOpen: any
  nodeById: NodeById
  previewNode: NodeObject
  setPreviewNode: any
  linksByNodeId: LinksByNodeId
}

const Sidebar = (props: SidebarProps) => {
  const { isOpen, onOpen, onClose, previewNode, setPreviewNode, nodeById, linksByNodeId } = props

  const { highlightColor } = useContext(ThemeContext)
  const [previewRoamNode, setPreviewRoamNode] = useState<OrgRoamNode>()

  const getText = (id: string, setText: any) => {
    fetch(`http://localhost:35901/note/${id}`)
      .then((res) => {
        return res.text()
      })
      .then((res) => setText(res))
      .catch((e) => {
        return (
          'Could not fetch the text for some reason, sorry!\n\n This can happen because you have an id with forward slashes (/) in it.',
          console.log(e)
        )
      })
  }

  useEffect(() => {
    if (!previewNode.id) {
      onClose()
      return
    }
    onOpen()
    setPreviewRoamNode(previewNode as OrgRoamNode)
  }, [previewNode.id])

  const [justification, setJustification] = useState(0)
  const justificationList = ['justify', 'start', 'end', 'center']
  const [font, setFont] = useState('sans serif')
  const [indent, setIndent] = useState(0)
  //maybe want to close it when clicking outside, but not sure
  //const outsideClickRef = useRef();
  return (
    <Slide direction="right" in={isOpen} style={{ maxWidth: '50%' }} unmountOnExit>
      <Flex flexDirection="row" height="100%">
        <Box pl={2} color="gray.800" bg="alt.100" w="100%" paddingBottom={15}>
          <Flex
            justifyContent="space-between"
            paddingTop={4}
            pl={0}
            pb={5}
            pr={2}
            alignItems="center"
            color="black"
          >
            <Flex>
              <IconButton
                variant="link"
                size="md"
                icon={<ChevronLeftIcon />}
                aria-label="Previous node"
              />
              <Heading size="md" fontWeight={400}>
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
            <VStack height="100%" alignItems="left" bg="alt.100" paddingLeft={10}>
              <Box
                pr={8}
                overflow="scroll"
                height="85%"
                className="org"
                sx={{
                  '.katex': { overflowX: 'scroll' },
                  h1: { fontSize: '20', fontWeight: 'bold', marginBottom: 3 },
                  h2: {
                    fontSize: '18',
                    marginBottom: 2,
                    color: 'black',
                  },
                  h3: {
                    fontSize: '16',
                    fontWeight: '600 !important',
                    marginBottom: '.5em',
                  },
                  h4: {
                    fontSize: '14',
                    fontWeight: '500 !important',
                    marginBottom: '.25em',
                    fontStyle: 'italic',
                  },
                  a: {
                    color: highlightColor,
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
                  div: {
                    fontSize: 'small',
                    hyphens: 'auto !important',
                    textAlign: justificationList[justification],
                  },
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
                  // org-like indents
                  'h1, h1 ~ *,h2 ~ h1,h2 ~ h1 ~ *,h3 ~ h1,h3 ~ h1 ~ *': {
                    marginLeft: 0 * indent,
                  },
                  'h2 ~ *, h1 ~ h2,h1 ~ h2 ~ *:not(h1):not(h3)': {
                    marginLeft: 2 * indent,
                  },
                  'h3 ~ *,h1 ~ h3,h1 ~ h3 ~ *:not(h1):not(h2)': {
                    marginLeft: 4 * indent,
                  },
                }}
              >
                {previewNode?.id && (
                  <Box>
                    <Flex pt={1} alignItems="center" justifyContent="flex-end">
                      <IconButton
                        variant="ghost"
                        aria-label="Justify content"
                        icon={
                          [
                            <BiAlignJustify key="justify" />,
                            <BiAlignLeft key="left" />,
                            <BiAlignRight key="right" />,
                            <BiAlignMiddle key="center" />,
                          ][justification]
                        }
                        onClick={() => setJustification((curr) => (curr + 1) % 4)}
                      />
                      <IconButton
                        variant="ghost"
                        aria-label="Indent Text"
                        icon={<BiRightIndent />}
                        onClick={() => {
                          console.log(indent)
                          setIndent((curr: number) => (indent ? 0 : 1))
                        }}
                      />
                      <IconButton
                        variant="ghost"
                        aria-label="Change font"
                        icon={<BiFont />}
                        onClick={() => {
                          setFont((curr: string) =>
                            curr === 'sans serif' ? 'serif' : 'sans serif',
                          )
                        }}
                      />
                    </Flex>
                    <Flex height="100%" flexDirection="column" justifyContent="space-between">
                      <UniOrg
                        {...{
                          getText,
                          setPreviewNode,
                          previewNode,
                          nodeById,
                        }}
                      />
                      <Backlinks
                        {...{
                          setPreviewNode,
                          previewNode,
                          nodeById,
                          linksByNodeId,
                          getText,
                        }}
                      />
                    </Flex>
                  </Box>
                )}
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
