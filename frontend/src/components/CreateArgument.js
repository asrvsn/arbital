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
    const { claim, claimItems } = this.props

    const dataSource = claimItems.map(item => ({
      text: item.claimText,
      value: (
        <AuthoredListItem
          text={item.claimText}
          authorId={item.claimAuthorId}
          authorName={item.claimAuthorName}
          hrefPath={'/claims/' + item.claimId}
        />
      )
    }))

    return (
      <Dialog
        title="New argument"
        actions={actions}
        modal={false}
        open={open}
        onRequestClose={() => this.close()}
      >
        { claim === undefined ?
            <AutoComplete
              floatingLabelText="Link to a claim"
              filter={AutoComplete.fuzzyFilter}
              dataSource={dataSource}
              maxSearchResults={5}
            />
          :
            <AuthoredListItem
              text={claim.claimText}
              authorId={claim.claimAuthorId}
              authorName={claim.claimAuthorName}
              hrefPath={'/claims/' + claim.claimId}
            />
        }

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
  (props) => {
    const { claimId } = props
    if (claimId === undefined) {
      return [
        {
          path: '/claims/items',
          mapResponseToProps: (resp) => {claimItems: JSON.parse(resp.body)}
        }
      ]
    } else {
      return [
        {
          path: '/claims/items/' + claimId,
          mapResponseToProps: (resp) => {claim: JSON.parse(resp.body)}
        }
      ]
    }
  }
)
