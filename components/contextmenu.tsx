import React, { useRef } from 'react'
import {
  Box,
  Menu,
  MenuItem,
  MenuList,
  MenuGroup,
  MenuItemOption,
  MenuOptionGroup,
  Heading,
  MenuDivider,
  Modal,
  ModalOverlay,
  ModalContent,
  ModalHeader,
  ModalFooter,
  ModalBody,
  ModalCloseButton,
  useDisclosure,
  Button,
  PopoverTrigger,
  PopoverContent,
  Popover,
  Flex,
  PopoverBody,
  PopoverCloseButton,
  PopoverArrow,
  PopoverHeader,
  PopoverFooter,
  Portal,
  Text,
  VStack,
} from '@chakra-ui/react'
import {
  DeleteIcon,
  EditIcon,
  CopyIcon,
  AddIcon,
  ViewIcon,
  ExternalLinkIcon,
  ChevronRightIcon,
  PlusSquareIcon,
} from '@chakra-ui/icons'

import { OrgRoamGraphReponse, OrgRoamLink, OrgRoamNode } from '../api'

export default interface ContextMenuProps {
  background: Boolean
  node?: OrgRoamNode
  nodeType?: string
  coordinates: number[]
  handleLocal: (node: OrgRoamNode, add: string) => void
  openNodeInEmacs: (node: OrgRoamNode) => void
  menuClose: () => void
  scope: { nodeIds: string[] }
  deleteNodeInEmacs: (node: OrgRoamNode) => void
  createNodeInEmacs: (node: OrgRoamNode) => void
}

export const ContextMenu = (props: ContextMenuProps) => {
  const {
    background,
    node,
    nodeType,
    coordinates,
    handleLocal,
    menuClose,
    scope,
    openNodeInEmacs,
    deleteNodeInEmacs,
    createNodeInEmacs,
  } = props
  const { isOpen, onOpen, onClose } = useDisclosure()
  const copyRef = useRef<any>()
  return (
    <>
      <Box
        position="absolute"
        zIndex="overlay"
        left={coordinates[0] + 10}
        top={coordinates[1] - 10}
        padding={5}
      >
        <Menu closeOnBlur={false} defaultIsOpen onClose={() => menuClose()}>
          <MenuList zIndex="overlay" bgColor="alt.100" borderColor="gray.500" maxWidth="xs">
            {node && (
              <>
                <Heading size="sm" isTruncated px={3} py={1}>
                  {node.title}
                </Heading>
                <MenuDivider borderColor="gray.500" />
              </>
            )}
            {scope.nodeIds.length !== 0 && (
              <>
                <MenuItem onClick={() => handleLocal(node!, 'add')} icon={<PlusSquareIcon />}>
                  Expand local graph at node
                </MenuItem>
                <MenuItem onClick={() => handleLocal(node!, 'replace')} icon={<ViewIcon />}>
                  Open local graph for this node
                </MenuItem>
              </>
            )}
            {!node?.properties.FILELESS ? (
              <MenuItem icon={<EditIcon />} onClick={() => openNodeInEmacs(node as OrgRoamNode)}>
                Open in Emacs
              </MenuItem>
            ) : (
              <MenuItem icon={<AddIcon />} onClick={() => createNodeInEmacs(node)}>
                Create node
              </MenuItem>
            )}
            {node?.properties.ROAM_REFS && (
              <MenuItem icon={<ExternalLinkIcon />}>Open in Zotero</MenuItem>
            )}
            {scope.nodeIds.length === 0 && (
              <MenuItem icon={<ViewIcon />} onClick={() => handleLocal(node!, 'replace')}>
                Open local graph
              </MenuItem>
            )}
            {/* Doesn't work at the moment
                            <MenuItem closeOnSelect={false} closeOnBlur={false}>
                            <Box _hover={{ bg: 'gray.200' }} width="100%">
                                <Popover
                                    initialFocusRef={copyRef}
                                    trigger="hover"
                                    placement="right-start"
                                    gutter={0}
                                >
                                    <PopoverTrigger>
                                        <MenuItem closeOnSelect={false} icon={<CopyIcon />}>
                                            <Flex justifyContent="space-between" alignItems="center">
                                                Copy...
                                                <ChevronRightIcon />
                                            </Flex>
                                        </MenuItem>
                                    </PopoverTrigger>
                                    <PopoverContent width={100}>
                                        <Menu defaultIsOpen closeOnBlur={false} closeOnSelect={false}>
                                            <MenuList bg="alt.100" zIndex="popover">
                                                <MenuItem ref={copyRef}>ID</MenuItem>
                                                <MenuItem>Title</MenuItem>
                                                <MenuItem>File path</MenuItem>
                                            </MenuList>
                                        </Menu>
                                    </PopoverContent>
                                </Popover>
                            </Box>
                        </MenuItem> */}
            {node?.level === 0 && (
              <MenuItem
                closeOnSelect={false}
                icon={<DeleteIcon color="red.500" />}
                color="red.500"
                onClick={onOpen}
              >
                Permenantly delete note
              </MenuItem>
            )}
          </MenuList>
        </Menu>
      </Box>
      <Modal isCentered isOpen={isOpen} onClose={onClose}>
        <ModalOverlay />
        <ModalContent zIndex="popover">
          <ModalHeader>Delete node?</ModalHeader>
          <ModalCloseButton />
          <ModalBody>
            <VStack spacing={4} display="flex" alignItems="flex-start">
              <Text>This will permanently delete your note:</Text>
              <Text fontWeight="bold">{node?.title}</Text>
              {node?.level !== 0 && (
                <Text>
                  This will only delete the from this heading until but not including the next node.
                  Your parent file and all other nodes will not be deleted.
                </Text>
              )}
              <Text>Are you sure you want to do continue?</Text>
            </VStack>
          </ModalBody>
          <ModalFooter>
            <Button
              mr={3}
              onClick={() => {
                console.log('closing')
                onClose()
                menuClose()
              }}
            >
              Cancel
            </Button>
            <Button
              variant="link"
              colorScheme="red"
              ml={3}
              onClick={() => {
                console.log('aaaaa')
                deleteNodeInEmacs(node!)
                onClose()
                menuClose()
              }}
            >
              Delete node
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  )
}

/* <Box>
 *     <Popover>
 *         <PopoverTrigger>
 *                 Permenantly delete node
 *             </MenuItem>
 *         </PopoverTrigger>
 *         <PopoverContent borderColor="red.500" _focus={{}}>
 *             <PopoverHeader fontWeight="semibold">Delete Node?</PopoverHeader>
 *             <PopoverArrow />
 *             <PopoverCloseButton onClick={onClose} />
 *             <PopoverBody>
 *                 This will permanently delete your node! Are you sure you want to do this?
 *             </PopoverBody>
 *             <PopoverFooter>
 *                 <Flex justifyContent="space-between" py={1}>
 *                     <Button colorScheme="gray" bg="gray.800" color="alt.100" width={30} onClick={onClose}>
 *                         Nah
 *                     </Button>
 *                     <Button colorScheme="red" variant="link" onClick={onClose}>
 *                         Delete node
 *                     </Button>
 *                 </Flex>
 *             </PopoverFooter>
 *         </PopoverContent>
 *     </Popover>
 * </Box> */
