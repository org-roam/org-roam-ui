import { IconButton, Modal, ModalOverlay, Tooltip, useDisclosure } from '@chakra-ui/react'
import { SearchIcon } from '@chakra-ui/icons'
import { NodeObject } from 'force-graph'
import { SearchContent } from './SearchContent'
import { useCallback, useEffect, useRef, useState } from 'react'
import { NodeById } from '../../pages'

export const Search: React.FC<{
  nodeById: NodeById
  setPreviewNode: (newPresent: NodeObject) => void
  onClickResultItem?: (nodeId: string) => void
}> = ({ nodeById, setPreviewNode, onClickResultItem }) => {
  const { isOpen, onOpen, onClose } = useDisclosure()

  // Full note text isn't part of the graph data Emacs sends over the
  // websocket (it's only kept in the files on disk), so it's fetched
  // lazily from org-roam-ui's own `/node/:id` servlet the first time
  // search is opened, then cached here (above <Modal>, so it survives
  // the modal unmounting) and topped up for any newly-seen node ids.
  const [contentById, setContentById] = useState<{ [id: string]: string }>({})
  const fetchedIdsRef = useRef<Set<string>>(new Set())
  const [contentLoading, setContentLoading] = useState(false)

  // `nodeById' gets a new object every time Emacs pushes fresh graph data
  // (on every org-roam save, since `org-roam-ui-update-on-save' is on) --
  // drop the cached content so a just-edited note's search index isn't
  // stale. The next effect below lazily refetches once search is open.
  const prevNodeByIdRef = useRef(nodeById)
  useEffect(() => {
    if (prevNodeByIdRef.current === nodeById) {
      return
    }
    prevNodeByIdRef.current = nodeById
    fetchedIdsRef.current = new Set()
    setContentById({})
  }, [nodeById])

  useEffect(() => {
    if (!isOpen) {
      return
    }
    const missingIds = Object.keys(nodeById).filter((id) => !fetchedIdsRef.current.has(id))
    if (!missingIds.length) {
      return
    }
    missingIds.forEach((id) => fetchedIdsRef.current.add(id))
    setContentLoading(true)
    Promise.all(
      missingIds.map((id) =>
        fetch(`/node/${encodeURIComponent(id)}`)
          .then((res) => (res.ok ? res.text() : Promise.reject(res.status)))
          .then((text) => [id, text] as const)
          // A null text marks a failed fetch.
          .catch(() => [id, null] as const),
      ),
    ).then((entries) => {
      setContentById((current) => {
        const next = { ...current }
        entries.forEach(([id, text]) => {
          if (text === null) {
            // Retry on the next time search is opened.
            fetchedIdsRef.current.delete(id)
            return
          }
          next[id] = text
        })
        return next
      })
      setContentLoading(false)
    })
  }, [isOpen, nodeById])

  const handleOnClickResultItem = useCallback(
    (id: string) => {
      const node = nodeById[id]
      onClose()
      if (!!node) {
        setPreviewNode(node)
        history.replaceState(null, '', window.location.pathname + `#${node.id}`)
        if (onClickResultItem) onClickResultItem(node.id)
      }
    },
    [nodeById, onClickResultItem, onClose, setPreviewNode],
  )

  return (
    <>
      <Tooltip label="Search node">
        <IconButton
          m={1}
          aria-label="Search node"
          variant="subtle"
          icon={<SearchIcon />}
          onClick={(e) => {
            e.currentTarget.blur()
            onOpen()
          }}
        />
      </Tooltip>
      <Modal isOpen={isOpen} onClose={onClose}>
        <ModalOverlay />
        <SearchContent
          nodeById={nodeById}
          contentById={contentById}
          contentLoading={contentLoading}
          onClickResultItem={handleOnClickResultItem}
        />
      </Modal>
    </>
  )
}
