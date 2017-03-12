import React, { Component } from 'react'
import requests from 'requests'

import Dialog from 'material-ui/Dialog'
import AutoComplete from 'material-ui/AutoComplete';

import GetterHOC from '../hoc/GetterHOC'
import AuthoredListItem from './AuthoredListItem';

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
    // TODO submit argumennt to backend
  }
}

export default GetterHOC(
  CreateArgument,
  (props) => ([
    {
      path: '/claims/items' + props.claimId,
      mapResponseToProps: (resp) => {claimItem: JSON.parse(resp.body)}
    }
  ])
)
