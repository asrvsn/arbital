import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import ContentAddBox from 'material-ui/svg-icons/content/add-box';
import Chip from 'material-ui/Chip';
import FlatButton from 'material-ui/FlatButton';
import TextField from 'material-ui/TextField';
import DropDownMenu from 'material-ui/DropDownMenu';
import MenuItem from 'material-ui/MenuItem';
import RaisedButton from 'material-ui/RaisedButton';
import Divider from 'material-ui/Divider';
import {List, ListItem} from 'material-ui/List';

import GetterHOC from '../hoc/GetterHOC'
import AuthoredListItem from '../items/AuthoredListItem';
import Finder from '../Finder'

import backend from '../../util/backend'

const styles = {
  firstRow: {
    display: 'flex',
    flexDirection: 'row',
    height: 20
  },
}

class CreateArgument extends Component {

  constructor(props) {
    super(props)
    this.state = {
      openChild: null,
      supportingClaims: [],
      arg: null,
      isFor: true,
      dynLinkedClaim: null,
      submitEnabled: false
    }
  }

  componentDidMount() {
    const { linkedClaim } = this.props
    this.setState({ dynLinkedClaim: linkedClaim })
  }

  render() {
    const { open } = this.props
    const { openChild, supportingClaims, arg, isFor, dynLinkedClaim } = this.state

    console.warn("supportingClaims", supportingClaims)

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
      />,
    ]

    return (
      <Dialog
        title="New argument"
        actions={actions}
        modal={false}
        open={open}
        onRequestClose={() => this.close()}
        autoScrollBodyContent={true}
      >

        <TextField
          hintText="Enter argument summary"
          ref={elem => this.argTextElem = elem}
          fullWidth={true}
        />

        <br />

        <div style={styles.firstRow}>

          <DropDownMenu
            value={isFor ? 1 : 2}
            onChange={(e, i, v) => this.setIsFor(v == 1)}
          >
            <MenuItem value={1} primaryText="For" />
            <MenuItem value={2} primaryText="Against" />
          </DropDownMenu>

          { dynLinkedClaim ?
              <Chip>{dynLinkedClaim.text}</Chip>
            :
              <RaisedButton
                label={openChild === "linkToClaim" ? "Cancel" : "Link to a claim"}
                labelPosition="after"
                primary={true}
                icon={<ContentAddBox />}
                onTouchTap={e => this.toggleOpenChild("linkToClaim")}
              />
          }

        </div>

        <br />
        <Divider />
        <br />

        <RaisedButton
          label={openChild === "addSupportingClaim" ? "Cancel" : "Add a supporting claim"}
          labelPosition="after"
          backgroundColor="green"
          icon={<ContentAddBox />}
          onTouchTap={e => this.toggleOpenChild("addSupportingClaim")}
        />

        { (openChild !== null) &&
            <div>
              <Divider />
              <Finder
                optionsAvailable={false}
                searchType="claims"
                onRequestClose={claim => this.closeChild(claim)}
              />
              <Divider />
            </div>
        }

        <List>
          { supportingClaims.map(claim =>
              <AuthoredListItem
                key={claim.id}
                text={claim.text}
                authorId={claim.authorId}
                authorName={claim.authorName}
              />
            )
          }
        </List>

      </Dialog>
    )
  }

  setIsFor(v) {
    this.setState({isFor: v})
  }

  getArgCreator() {
    const { supportingClaims } = this.state
    const text = this.argTextElem.input.value
    return {
      text,
      claims: supportingClaims.map(claim => claim.id)
    }
  }

  close(arg) {
    this.props.onRequestClose(arg)
  }

  submit(cb = (arg) => {}) {
    const argCreator = this.getArgCreator()
    if (argCreator.text === "" || argCreator.claims.length == 0) {
      return
    }

    const { dynLinkedClaim, isFor } = this.state
    const { session } = this.props
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

  toggleOpenChild(child) {
    this.setState({
      openChild: child === this.state.openChild ? null : child
    })
  }

  closeChild(claim) {
    const { openChild, dynLinkedClaim, supportingClaims } = this.state

    switch(openChild) {
      case 'linkToClaim': {
        this.setState({
          openChild: null,
          dynLinkedClaim: claim || dynLinkedClaim
        })
        return
      }
      case 'addSupportingClaim': {
        if (claim !== null) {
          supportingClaims.push(claim)
        }
        this.setState({
          openChild: null,
          supportingClaims
        })
        return
      }
      default: {
        this.setState({openChild: null})
        return
      }
    }
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
