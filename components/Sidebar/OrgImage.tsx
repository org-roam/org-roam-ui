import React, { useEffect, useState } from 'react'
import Image from 'next/image'
import path from 'path'
//import '../../../public/placeholder.png'

export interface OrgImageProps {
  src: string
  file: string
}

export const OrgImage = (props: OrgImageProps) => {
  const { src, file } = props

  const [image, setImage] = useState<any>(null)
  const dir = path.dirname(file)
  const fullPath = encodeURIComponent(encodeURIComponent(path.join(dir, src)))

  /* )
*   .then((res) => res.blob())
*   .then((res) => setImage(res))
*   .catch((e) => {
*     setImage(null)
*     console.error(e)
*   })
}, [fullPath]) */

  return <Image src={`http://localhost:35901/img/${fullPath}`} alt="" width={100} height={100} />
}
