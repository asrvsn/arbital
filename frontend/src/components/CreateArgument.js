import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import AutoComplete from 'material-ui/AutoComplete';

import AuthenticatedHOC from '../hoc/AuthenticatedHOC'
import AuthoredListItem from './AuthoredListItem';

import backend from '../util/backend'

class CreateArgument extends Component {

  constructor(props) {
    super(props)
    this.state = {
      open: true
    }
  }

  render() {
    const { open } = this.state
    const { claimItem } = this.props

    return (
      <Dialog
        title="New argument"
        actions={actions}
        modal={false}
        open={open}
        onRequestClose={() => this.close()}
      >
        <AuthoredListItem
          text={claimItem.claimText}
          authorId={claimItem.claimAuthorId}
          authorName={claimItem.claimAuthorName}
          hrefPath={'/claims/' + claimItem.claimId}
        />

      </Dialog>
    )
  }

  close() {
    this.setState({open: false})
  }

  submit() {
    const { sessionId, claimItem } = this.props
    const path = `/claims/${claimItem.claimId}/for` // TODO
    const argument = undefined // TODO
    backend
      .authenticate(sessionId)
      .post(path, argument)
      .on('response', resp => {
        // TODO
      })
  }
}

export default AuthenticatedHOC(CreateArgument)
