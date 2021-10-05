import React, { useEffect, useState } from 'react'
import { Button } from '@chakra-ui/react'

async function verifyPermission(fileHandle: any, readWrite: any) {
  const options: any = {}
  if (readWrite) {
    options.mode = 'readwrite'
  }
  // Check if permission was already granted. If so, return true.
  if ((await fileHandle.queryPermission(options)) === 'granted') {
    return true
  }
  // Request permission. If the user grants permission, return true.
  if ((await fileHandle.requestPermission(options)) === 'granted') {
    return true
  }
  // The user didn't grant permission, so return false.
  return false
}

export default function Testpage() {
  const [text, setText] = useState(0)
  const [dirHandle, setDirhandle] = useState<any>()
  const [perm, setPerm] = useState(false)

  const pick = async () => {
    const dirHandle = await window.showDirectoryPicker()
    console.log(dirHandle)
    setDirhandle(dirHandle)
  }

  useEffect(() => {
    ;(async () => {
      console.log(dirHandle)
      const newFileHandle = dirHandle ? await dirHandle.getFileHandle('inbox.org') : null
      const file = await newFileHandle.getFile()
      const ttext = await file.text()
      setText(ttext)
      const path = newFileHandle ? await dirHandle.resolve(newFileHandle) : null
      console.log(path)
    })()
  }, [dirHandle])

  return (
    <div>
      <Button onClick={() => pick()}> Press </Button>
      <p>{text}</p>
      <Button onClick={() => setPerm(verifyPermission(dirHandle, true))}>Check permission</Button>
      <p>{perm ? '👍' : '👎'}</p>
    </div>
  )
}
