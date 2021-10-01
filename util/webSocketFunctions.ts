import { OrgRoamNode } from '../api'
import ReconnectingWebSocket from 'reconnecting-websocket'

export function sendMessageToEmacs(command: string, data: {}, webSocket: ReconnectingWebSocket) {
    webSocket.send(JSON.stringify({ command: command, data: data }))
}

export function getOrgText(node: OrgRoamNode, webSocket: ReconnectingWebSocket) {
    sendMessageToEmacs('getText', { id: node.id }, webSocket)
}

export function openNodeInEmacs(node: OrgRoamNode, webSocket: ReconnectingWebSocket) {
    sendMessageToEmacs('open', { id: node.id }, webSocket)
}

export function deleteNodeInEmacs(node: OrgRoamNode, webSocket: ReconnectingWebSocket) {
    if (node.level !== 0) {
        return
    }
    sendMessageToEmacs('delete', { id: node.id, file: node.file }, webSocket)
}

export function createNodeInEmacs(node: OrgRoamNode, webSocket: ReconnectingWebSocket) {
    sendMessageToEmacs('create', { id: node.id, title: node.title, ref: node.properties.ROAM_REFS }, webSocket)
}
