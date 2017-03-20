import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import AutoComplete from 'material-ui/AutoComplete';
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import Chip from 'material-ui/Chip';

import GetterHOC from '../hoc/GetterHOC'
import AuthoredListItem from './AuthoredListItem';
import ClaimFinder from './ClaimFinder'

import backend from '../util/backend'

class CreateArgument extends Component {

  constructor(props) {
    super(props)
    this.state = {
      childOpen: false,
      claims: [],
      arg: null,
      isFor: true,
      dynLinkedClaim: null
    }
  }

  componentDidMount() {
    const { linkedClaim } = this.props
    this.setState({ dynLinkedClaim: linkedClaim })
  }

  render() {
    const { open } = this.props
    const { childOpen, claims, arg, isFor, dynLinkedClaim } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        primary={true}
        onTouchTap={() => this.close(null)}
      />,
      <FlatButton
        label="Submit"
        primary={true}
        onTouchTap={() => this.submit(arg => this.close(arg))}
        disabled={submitted}
      />,
    ]

    return (
      <Dialog
        title="New argument"
        actions={actions}
        modal={false}
        open={open}
        onRequestClose={() => this.close()}
      >
        <TextField
          hintText="Enter argument summary"
          ref={elem => this.argTextElem = elem}
        />
        <DropDownMenu
          value={isFor ? 'For' : 'Against'}
          onChange={(e, i, v) => this.setIsFor(v == 'For')}
        >
          <MenuItem value={1} primaryText="For" />
          <MenuItem value={2} primaryText="Against" />
        </DropDownMenu>
        { dynLinkedClaim ?
            <Chip>{dynLinkedClaim.text}</Chip>
          :
            <RaisedButton
              label="Link to claim"
              labelPosition="after"
              primary={true}
              icon={<ContentAddBox />}
              onTouchTap={e => this.openChild()}
            />
        }
        { childOpen &&
            <ClaimFinder
              onRequestClose={claim => this.closeChild(claim)}
            />
        }

      </Dialog>
    )
  }

  setIsFor(v) {
    this.setState({isFor: v})
  }

  getArgCreator() {
    const { claims } = this.state
    const text = this.argTextElem.input.value
    return {
      text,
      claims: claims.map(claim => claim.id)
    }
  }

  close(arg) {
    this.props.onRequestClose(arg)
  }

  submit(cb = (arg) => {}) {
    const { dynLinkedClaim, isFor } = this.state
    const { session } = this.props
    const argCreator = this.getArgCreator()
    const url = `/claims/${dynLinkedClaim.id}/${isFor ? 'for' : 'against'}`

    backend
      .authenticate(session.id)
      .post(url, argCreator, (err, response, body) => {
        if (err !== null) {
          throw err
        } else {
          if (response.statusCode == 200) {
            this.setState({arg: body})
            cb(body)
          } else {
            throw response.statusMessage
          }
        }
      })
  }

  openChild() {
    this.setState({ childOpen: true })
  }

  closeChild(linkedClaim) {
    this.setState({
      childOpen: false,
      dynLinkedClaim: linkedClaim
    })
  }
}

export default GetterHOC(
  CreateArgument,
  (props) => {
    if (props.linkedClaimId !== undefined) {
      return {
        claim: {
          path: '/claims/' + linkedClaimId,
          mapResponseToProps: (resp) => ({linkedClaim: resp})
        }
      }
    } else {
        return {}
    }
  }
)
